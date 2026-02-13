/*
 * ================================================================
 *  FALSE POSITION METHOD — QUADRATIC SOLVER  (server.c)
 *  Serves index.html and handles POST /solve API requests
 *
 *  Compile:  gcc -o server server.c -lm
 *  Run:      ./server
 *  Browser:  http://localhost:8080
 *
 *  Equation type: QUADRATIC ONLY  →  f(x) = ax² + bx + c
 * ================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <errno.h>
/* ────────────────────────────────────────
   CONSTANTS
──────────────────────────────────────── */
#define BACKLOG      10
#define BUF_SIZE     8192
#define MAX_ITER     200

/* ────────────────────────────────────────
   ONE ROW OF ITERATION TABLE
──────────────────────────────────────── */
typedef struct {
    int    n;
    double xl, xu, xr;
    double fxl, fxu, fxr;
    double ea;          /* approximate relative error % */
} FPRow;

/* ────────────────────────────────────────
   QUADRATIC  f(x) = a*x^2 + b*x + c
──────────────────────────────────────── */
static double fquad(double a, double b, double c, double x) {
    return a*x*x + b*x + c;
}

/* ────────────────────────────────────────
   URL DECODE
──────────────────────────────────────── */
static int hexd(char ch) {
    if (ch >= '0' && ch <= '9') return ch - '0';
    if (ch >= 'A' && ch <= 'F') return ch - 'A' + 10;
    if (ch >= 'a' && ch <= 'f') return ch - 'a' + 10;
    return 0;
}
static void url_decode(const char *s, char *d, int max) {
    int j = 0;
    for (int i = 0; s[i] && j < max-1; i++) {
        if (s[i]=='%' && s[i+1] && s[i+2]) {
            d[j++] = (char)(hexd(s[i+1])*16 + hexd(s[i+2]));
            i += 2;
        } else d[j++] = (s[i]=='+') ? ' ' : s[i];
    }
    d[j] = '\0';
}
static void get_param(const char *q, const char *key, char *out, int max) {
    char srch[64]; snprintf(srch, sizeof(srch), "%s=", key);
    const char *p = strstr(q, srch);
    if (!p) { out[0]='\0'; return; }
    p += strlen(srch);
    char raw[256]={0}; int i=0;
    while (*p && *p!='&' && i<255) raw[i++]=*p++;
    url_decode(raw, out, max);
}

/* ────────────────────────────────────────
   FALSE POSITION  (Regula Falsi)
  Returns number of rows; root in *root
──────────────────────────────────────── */
static int false_position(double a, double b, double c,
                          double xl, double xu, double tol,
                          FPRow *rows, double *root, char *err)
{
    double fxl = fquad(a,b,c,xl);
    double fxu = fquad(a,b,c,xu);

    if (fxl * fxu > 0.0) {
        strcpy(err, "f(xL) and f(xU) must have opposite signs. No root is bracketed in this interval.");
        return -1;
    }
    if (tol <= 0.0) tol = 1e-4;

    double xr_old = xl;
    int n;

    for (n = 0; n < MAX_ITER; n++) {
        /* False Position formula: xr = xu - fxu*(xl-xu)/(fxl-fxu) */
        double denom = fxl - fxu;
        if (fabs(denom) < 1e-15) {
            strcpy(err, "Denominator too small — function values at xL and xU are nearly equal.");
            return -1;
        }
        double xr  = xu - fxu * (xl - xu) / denom;
        double fxr = fquad(a, b, c, xr);

        double ea = (n == 0) ? 100.0 : fabs((xr - xr_old) / xr) * 100.0;

        rows[n].n   = n + 1;
        rows[n].xl  = xl;
        rows[n].xu  = xu;
        rows[n].xr  = xr;
        rows[n].fxl = fxl;
        rows[n].fxu = fxu;
        rows[n].fxr = fxr;
        rows[n].ea  = ea;

        /* converged? */
        if (fabs(fxr) < 1e-14 || (n > 0 && ea < tol)) {
            *root = xr;
            return n + 1;
        }

        /* update bracket */
        if (fxl * fxr < 0.0) { xu = xr; fxu = fxr; }
        else                  { xl = xr; fxl = fxr; }

        xr_old = xr;
    }
    *root = rows[n-1].xr;
    return n;
}

/* ────────────────────────────────────────
   DISCRIMINANT CHECK & REAL ROOTS
──────────────────────────────────────── */
static int discriminant_info(double a, double b, double c,
                              double *r1, double *r2)
{
    /* returns: 2 = two real roots, 1 = one repeated, 0 = complex */
    double disc = b*b - 4.0*a*c;
    if (disc > 0.0) {
        *r1 = (-b + sqrt(disc)) / (2.0*a);
        *r2 = (-b - sqrt(disc)) / (2.0*a);
        return 2;
    } else if (fabs(disc) < 1e-12) {
        *r1 = *r2 = -b / (2.0*a);
        return 1;
    }
    return 0; /* complex roots */
}

/* ────────────────────────────────────────
   BUILD GRAPH JSON  [{x,y}, ...]
──────────────────────────────────────── */
static void graph_json(double a, double b, double c,
                       double xl, double xu,
                       char *buf, int max)
{
    double span = fabs(xu - xl);
    double gxl  = xl - span * 0.5;
    double gxr  = xu + span * 0.5;
    int    steps = 200;
    double step  = (gxr - gxl) / steps;
    int pos = snprintf(buf, max, "[");
    for (int i = 0; i <= steps && pos < max-80; i++) {
        double x = gxl + i * step;
        double y = fquad(a, b, c, x);
        if (i) pos += snprintf(buf+pos, max-pos, ",");
        pos += snprintf(buf+pos, max-pos,
                        "{\"x\":%.6f,\"y\":%.6f}", x, y);
    }
    snprintf(buf+pos, max-pos, "]");
}

/* ────────────────────────────────────────
   BUILD FULL JSON RESPONSE
──────────────────────────────────────── */
static void build_json(double ca, double cb, double cc,
                       double xl_orig, double xu_orig,
                       const FPRow *rows, int nrows,
                       double root,
                       char *out, int maxout)
{
    /* graph */
    static char gbuf[65536];
    graph_json(ca, cb, cc, xl_orig, xu_orig, gbuf, sizeof(gbuf));

    /* table */
    static char tbuf[65536];
    int tp = snprintf(tbuf, sizeof(tbuf), "[");
    for (int i = 0; i < nrows && tp < (int)sizeof(tbuf)-300; i++) {
        if (i) tp += snprintf(tbuf+tp, sizeof(tbuf)-tp, ",");
        tp += snprintf(tbuf+tp, sizeof(tbuf)-tp,
            "{\"n\":%d,"
             "\"xl\":%.8f,\"xu\":%.8f,\"xr\":%.8f,"
             "\"fxl\":%.8f,\"fxu\":%.8f,\"fxr\":%.8f,"
             "\"ea\":%.6f}",
            rows[i].n,
            rows[i].xl, rows[i].xu, rows[i].xr,
            rows[i].fxl, rows[i].fxu, rows[i].fxr,
            rows[i].ea);
    }
    tp += snprintf(tbuf+tp, sizeof(tbuf)-tp, "]");

    /* discriminant */
    double r1=0, r2=0;
    int dtype = discriminant_info(ca, cb, cc, &r1, &r2);
    char disc_str[128];
    double disc_val = cb*cb - 4.0*ca*cc;
    if (dtype == 2)
        snprintf(disc_str, sizeof(disc_str),
                 "Two distinct real roots (Δ = %.6f > 0)", disc_val);
    else if (dtype == 1)
        snprintf(disc_str, sizeof(disc_str),
                 "One repeated real root (Δ = 0)");
    else
        snprintf(disc_str, sizeof(disc_str),
                 "Complex roots (Δ = %.6f < 0) — no real root in interval", disc_val);

    /* verification */
    double froot = fquad(ca, cb, cc, root);
    double last_ea = (nrows > 1) ? rows[nrows-1].ea : 100.0;

    snprintf(out, maxout,
        "{"
        "\"success\":true,"
        "\"root\":%.10f,"
        "\"froot\":%.10e,"
        "\"iterations\":%d,"
        "\"last_ea\":%.6f,"
        "\"disc\":\"%s\","
        "\"r1\":%.8f,\"r2\":%.8f,\"dtype\":%d,"
        "\"graph\":%s,"
        "\"table\":%s"
        "}",
        root, froot, nrows, last_ea,
        disc_str, r1, r2, dtype,
        gbuf, tbuf);
}

/* ────────────────────────────────────────
   READ HTML FILE
──────────────────────────────────────── */
static long read_html(char **out) {
    FILE *f = fopen("index.html", "r");
if (!f) {
    fprintf(stderr, "==> ERROR: Cannot open index.html - %s\n", strerror(errno));
    return -1;
}
    fseek(f, 0, SEEK_END); long sz = ftell(f); rewind(f);
    *out = malloc(sz + 1);
    if (!*out) { fclose(f); return -1; }
    fread(*out, 1, sz, f); (*out)[sz] = '\0';
    fclose(f); return sz;
}

/* ────────────────────────────────────────
   HANDLE GET /  →  serve index.html
──────────────────────────────────────── */
static void handle_get(int fd) {
    char *html = NULL; long sz = read_html(&html);
    if (sz < 0) {
        const char *e = "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nNot Found";
        write(fd, e, strlen(e)); return;
    }
    char hdr[256];
    snprintf(hdr, sizeof(hdr),
        "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n"
        "Content-Length: %ld\r\n\r\n", sz);
    write(fd, hdr, strlen(hdr));
    write(fd, html, sz);
    free(html);
}

/* ────────────────────────────────────────
   HANDLE POST /solve  →  run & return JSON
──────────────────────────────────────── */
static void handle_solve(int fd, const char *body) {
    char tmp[128];
    get_param(body, "a",  tmp, sizeof(tmp)); double ca  = atof(tmp);
    get_param(body, "b",  tmp, sizeof(tmp)); double cb  = atof(tmp);
    get_param(body, "c",  tmp, sizeof(tmp)); double cc  = atof(tmp);
    get_param(body, "xl",  tmp, sizeof(tmp)); double xl  = atof(tmp);
    get_param(body, "xu",  tmp, sizeof(tmp)); double xu  = atof(tmp);
    get_param(body, "tol", tmp, sizeof(tmp)); double tol = atof(tmp);

    static FPRow rows[MAX_ITER];
    double root = 0.0;
    char   errmsg[256] = {0};

    static char jbody[131072];

    if (fabs(ca) < 1e-10) {
        snprintf(jbody, sizeof(jbody),
            "{\"success\":false,\"error\":"
            "\"Coefficient 'a' cannot be zero — that would not be a quadratic equation.\"}");
    } else {
        int n = false_position(ca, cb, cc, xl, xu, tol, rows, &root, errmsg);
        if (n < 0) {
            snprintf(jbody, sizeof(jbody),
                "{\"success\":false,\"error\":\"%s\"}", errmsg);
        } else {
            build_json(ca, cb, cc, xl, xu, rows, n, root, jbody, sizeof(jbody));
        }
    }

    char hdr[256];
    snprintf(hdr, sizeof(hdr),
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "Content-Length: %zu\r\n\r\n", strlen(jbody));
    write(fd, hdr, strlen(hdr));
    write(fd, jbody, strlen(jbody));
}

/* ────────────────────────────────────────
   MAIN
──────────────────────────────────────── */
int main(void) {
    char cwd[1024];
    getcwd(cwd, sizeof(cwd));
    fprintf(stderr, "==> DEBUG: Working directory is: %s\n", cwd);
    char *port_env = getenv("PORT");
    int PORT = port_env ? atoi(port_env) : 8080;
fprintf(stderr, "==> DEBUG: PORT from env='%s', using PORT=%d\n", port_env ? port_env : "NULL", PORT);
    int sfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sfd < 0) { perror("socket"); return 1; }
    int opt = 1;
    setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family      = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port        = htons(PORT);

    if (bind(sfd,(struct sockaddr*)&addr,sizeof(addr))<0){perror("bind");return 1;}
fprintf(stderr, "==> DEBUG: Successfully bound to 0.0.0.0:%d\n", PORT);
    if (listen(sfd,BACKLOG)<0){perror("listen");return 1;}
fprintf(stderr, "==> DEBUG: Now listening for connections\n");

    printf("╔══════════════════════════════════════════╗\n");
    printf("║  False Position Method Server RUNNING    ║\n");
    printf("║  Open: http://localhost:%d              ║\n", PORT);
    printf("╚══════════════════════════════════════════╝\n");

    while (1) {
        int cfd = accept(sfd, NULL, NULL);
	fprintf(stderr, "==> DEBUG: Connection accepted, fd=%d\n", cfd);
        if (cfd < 0) continue;
        static char req[BUF_SIZE];
        int n = read(cfd, req, BUF_SIZE-1);
        if (n <= 0) { close(cfd); continue; }
        req[n] = '\0';
        char method[8]={0}, path[64]={0};
        sscanf(req, "%7s %63s", method, path);
        if (strcmp(method,"GET")==0) {
            handle_get(cfd);
	fprintf(stderr, "==> DEBUG: Calling request handler\n");
        } else if (strcmp(method,"POST")==0 && strncmp(path,"/solve",6)==0) {
            char *b = strstr(req,"\r\n\r\n");
            handle_solve(cfd, b ? b+4 : "");
        }
        close(cfd);
    }
    close(sfd);
    return 0;
}
