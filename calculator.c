/*
 * calculator.c - TUI & CLI calculator
 * v1.0 by Cycl0o0
 *
 * cc -o calculator calculator.c -lncursesw -lm
 */

#define _XOPEN_SOURCE 700
#include <ncurses.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <locale.h>

#define VER        "Calculator v1.0  Made by Cycl0o0"
#define MAXBUF     256
#define NHIST      10
#define NCATS      8
#define MAXMAT     4
#define NMODES     7

enum { M_CALC, M_SCI, M_CONV, M_BASE, M_GRAPH, M_MAT, M_GCD };

// --- recursive descent expression parser ---

typedef struct {
    const char *src;
    int pos, deg, err, xused;
    double xv;
} Expr;

static double expr(Expr *e);

static void eatws(Expr *e) { while (e->src[e->pos] == ' ') e->pos++; }

static double number(Expr *e)
{
    eatws(e);
    double v = 0;
    int got = 0;
    while (isdigit((unsigned char)e->src[e->pos])) {
        v = v * 10 + (e->src[e->pos++] - '0');
        got = 1;
    }
    if (e->src[e->pos] == '.') {
        e->pos++;
        double f = 0.1;
        while (isdigit((unsigned char)e->src[e->pos])) {
            v += (e->src[e->pos++] - '0') * f;
            f *= 0.1;
            got = 1;
        }
    }
    if (!got) e->err = 1;
    return v;
}

static double torad(Expr *e, double v) { return e->deg ? v * M_PI / 180.0 : v; }
static double todeg(Expr *e, double v) { return e->deg ? v * 180.0 / M_PI : v; }

static double fact(double n)
{
    if (n < 0) return NAN;
    double r = 1;
    for (int i = 2; i <= (int)n; i++) r *= i;
    return r;
}

// atom: number | (expr) | func(expr) | const | x
static double atom(Expr *e)
{
    eatws(e);

    if (e->src[e->pos] == '(') {
        e->pos++;
        double v = expr(e);
        eatws(e);
        if (e->src[e->pos] == ')') e->pos++; else e->err = 1;
        return v;
    }

    if (isalpha((unsigned char)e->src[e->pos])) {
        char w[32]; int n = 0;
        while (isalpha((unsigned char)e->src[e->pos]) && n < 30)
            w[n++] = e->src[e->pos++];
        w[n] = '\0';

        if (!strcmp(w, "x")) { e->xused = 1; return e->xv; }
        if (!strcmp(w, "pi")) return M_PI;
        if (!strcmp(w, "e"))  return M_E;

        // must be a function call
        eatws(e);
        if (e->src[e->pos] != '(') { e->err = 1; return 0; }
        e->pos++;
        double a = expr(e);
        eatws(e);
        if (e->src[e->pos] == ')') e->pos++; else e->err = 1;

        if (!strcmp(w, "sin"))   return sin(torad(e, a));
        if (!strcmp(w, "cos"))   return cos(torad(e, a));
        if (!strcmp(w, "tan"))   return tan(torad(e, a));
        if (!strcmp(w, "asin"))  return todeg(e, asin(a));
        if (!strcmp(w, "acos"))  return todeg(e, acos(a));
        if (!strcmp(w, "atan"))  return todeg(e, atan(a));
        if (!strcmp(w, "log"))   return log10(a);
        if (!strcmp(w, "ln"))    return log(a);
        if (!strcmp(w, "exp"))   return exp(a);
        if (!strcmp(w, "sqrt"))  return sqrt(a);
        if (!strcmp(w, "cbrt"))  return cbrt(a);
        if (!strcmp(w, "abs"))   return fabs(a);
        if (!strcmp(w, "ceil"))  return ceil(a);
        if (!strcmp(w, "floor")) return floor(a);
        if (!strcmp(w, "fact"))  return fact(a);

        e->err = 1;
        return 0;
    }

    return number(e);
}

static double unary(Expr *e)
{
    eatws(e);
    if (e->src[e->pos] == '+') { e->pos++; return unary(e); }
    if (e->src[e->pos] == '-') { e->pos++; return -unary(e); }
    return atom(e);
}

static double power(Expr *e)
{
    double b = unary(e);
    eatws(e);
    if (e->src[e->pos] == '^') { e->pos++; return pow(b, power(e)); }
    return b;
}

static double term(Expr *e)
{
    double v = power(e);
    for (;;) {
        eatws(e);
        if (e->src[e->pos] == '*') {
            e->pos++; v *= power(e);
        } else if (e->src[e->pos] == '/') {
            e->pos++;
            double d = power(e);
            v = d ? v / d : NAN;
        } else if (e->src[e->pos] == '%') {
            e->pos++;
            double d = power(e);
            v = d ? fmod(v, d) : NAN;
        } else break;
    }
    return v;
}

static double expr(Expr *e)
{
    double v = term(e);
    for (;;) {
        eatws(e);
        if (e->src[e->pos] == '+')      { e->pos++; v += term(e); }
        else if (e->src[e->pos] == '-') { e->pos++; v -= term(e); }
        else break;
    }
    return v;
}

static int eval(const char *s, int deg, double *out)
{
    if (!s || !*s) return 0;
    Expr e = { s, 0, deg, 0, 0, 0 };
    *out = expr(&e);
    eatws(&e);
    return !(e.err || e.src[e.pos]);
}

static int eval_at(const char *s, int deg, double x, double *out)
{
    if (!s || !*s) return 0;
    Expr e = { s, 0, deg, 0, 0, x };
    *out = expr(&e);
    eatws(&e);
    return !(e.err || e.src[e.pos]);
}

static void fmtval(double v, char *buf, int sz)
{
    if (isnan(v)) { snprintf(buf, sz, "Error"); return; }
    if (isinf(v)) { snprintf(buf, sz, "Infinity"); return; }
    if (v == (long long)v && fabs(v) < 1e15)
        snprintf(buf, sz, "%lld", (long long)v);
    else
        snprintf(buf, sz, "%.10g", v);
}

// --- gcd/lcm stuff ---

static long long gcd(long long a, long long b)
{
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b) { long long t = b; b = a % b; a = t; }
    return a;
}

static long long lcm(long long a, long long b)
{
    if (!a || !b) return 0;
    return (a / gcd(a, b)) * (b < 0 ? -b : b);
}

static void factorize(long long n, char *buf, int sz)
{
    if (n <= 1) { snprintf(buf, sz, "%lld", n); return; }
    int p = 0, first = 1;
    long long orig = n;
    for (long long d = 2; d * d <= n; d++) {
        int cnt = 0;
        while (n % d == 0) { n /= d; cnt++; }
        if (!cnt) continue;
        if (!first) p += snprintf(buf + p, sz - p, " * ");
        p += (cnt == 1)
            ? snprintf(buf + p, sz - p, "%lld", d)
            : snprintf(buf + p, sz - p, "%lld^%d", d, cnt);
        first = 0;
        if (p >= sz - 1) return;
    }
    if (n > 1) {
        if (!first) p += snprintf(buf + p, sz - p, " * ");
        snprintf(buf + p, sz - p, "%lld", n);
    }
    if (first) snprintf(buf, sz, "%lld", orig);
}

// --- matrix ---

typedef struct { double d[MAXMAT][MAXMAT]; int r, c; } Mat;

static int mat_parse(const char *s, Mat *m)
{
    m->r = m->c = 0;
    if (!s || !*s) return 0;
    char tmp[MAXBUF];
    snprintf(tmp, sizeof(tmp), "%s", s);

    int ncols = 0;
    char *line = tmp, *sc;
    while (line && m->r < MAXMAT) {
        while (*line == ' ') line++;
        if (!*line) break;
        sc = strchr(line, ';');
        if (sc) *sc = '\0';

        int col = 0;
        char *p = line;
        while (*p && col < MAXMAT) {
            while (*p == ' ' || *p == ',') p++;
            if (!*p) break;
            char *end;
            double v = strtod(p, &end);
            if (end == p) break;
            m->d[m->r][col++] = v;
            p = end;
        }
        if (col) {
            if (!ncols) ncols = col;
            else if (col != ncols) return 0; // inconsistent row widths
            m->r++;
        }
        line = sc ? sc + 1 : NULL;
    }
    m->c = ncols;
    return m->r > 0 && m->c > 0;
}

static double determinant(const Mat *m);

static double minor_det(const Mat *m, int ri, int ci)
{
    Mat sub;
    sub.r = m->r - 1;
    sub.c = m->c - 1;
    int sr = 0;
    for (int i = 0; i < m->r; i++) {
        if (i == ri) continue;
        int sc = 0;
        for (int j = 0; j < m->c; j++) {
            if (j == ci) continue;
            sub.d[sr][sc++] = m->d[i][j];
        }
        sr++;
    }
    return determinant(&sub);
}

static double determinant(const Mat *m)
{
    if (m->r != m->c) return NAN;
    if (m->r == 1) return m->d[0][0];
    if (m->r == 2) return m->d[0][0]*m->d[1][1] - m->d[0][1]*m->d[1][0];
    double det = 0;
    for (int j = 0; j < m->c; j++)
        det += ((j & 1) ? -1.0 : 1.0) * m->d[0][j] * minor_det(m, 0, j);
    return det;
}

static void transpose(const Mat *m, Mat *o)
{
    o->r = m->c; o->c = m->r;
    for (int i = 0; i < m->r; i++)
        for (int j = 0; j < m->c; j++)
            o->d[j][i] = m->d[i][j];
}

static int invert(const Mat *m, Mat *o)
{
    if (m->r != m->c) return 0;
    double d = determinant(m);
    if (fabs(d) < 1e-15) return 0;
    int n = m->r;
    o->r = o->c = n;
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            o->d[j][i] = (((i+j) & 1) ? -1.0 : 1.0) * minor_det(m, i, j) / d;
    return 1;
}

static int mat_add(const Mat *a, const Mat *b, Mat *o)
{
    if (a->r != b->r || a->c != b->c) return 0;
    o->r = a->r; o->c = a->c;
    for (int i = 0; i < a->r; i++)
        for (int j = 0; j < a->c; j++)
            o->d[i][j] = a->d[i][j] + b->d[i][j];
    return 1;
}

static int mat_mul(const Mat *a, const Mat *b, Mat *o)
{
    if (a->c != b->r) return 0;
    o->r = a->r; o->c = b->c;
    for (int i = 0; i < o->r; i++)
        for (int j = 0; j < o->c; j++) {
            o->d[i][j] = 0;
            for (int k = 0; k < a->c; k++)
                o->d[i][j] += a->d[i][k] * b->d[k][j];
        }
    return 1;
}

static void fmtrow(const Mat *m, int row, char *buf, int sz)
{
    int p = snprintf(buf, sz, "|");
    for (int j = 0; j < m->c && p < sz - 10; j++) {
        char tmp[32]; fmtval(m->d[row][j], tmp, sizeof(tmp));
        p += snprintf(buf + p, sz - p, " %8s", tmp);
    }
    snprintf(buf + p, sz - p, " |");
}

// --- units ---

typedef struct { const char *nm; double factor; } UnitDef;
typedef struct { const char *label; const UnitDef *u; int n; int temp; } Category;

static const UnitDef u_temp[] = {{"C",1,}, {"F",1}, {"K",1}};
static const UnitDef u_len[] = {
    {"m",1}, {"km",1000}, {"mi",1609.344}, {"ft",0.3048},
    {"in",0.0254}, {"cm",0.01}, {"mm",0.001}, {"yd",0.9144}};
static const UnitDef u_wt[] = {
    {"kg",1}, {"g",0.001}, {"lb",0.453592}, {"oz",0.0283495}, {"ton",1000}};
static const UnitDef u_area[] = {
    {"m2",1}, {"km2",1e6}, {"ft2",0.092903}, {"acre",4046.86}, {"hectare",10000}};
static const UnitDef u_vol[] = {
    {"L",1}, {"mL",0.001}, {"gal",3.78541}, {"fl_oz",0.0295735}, {"cup",0.236588}};
static const UnitDef u_spd[] = {
    {"m/s",1}, {"km/h",0.277778}, {"mph",0.44704}, {"knot",0.514444}};
static const UnitDef u_data[] = {
    {"B",1}, {"KB",1024}, {"MB",1048576}, {"GB",1073741824},
    {"TB",1099511627776.0}, {"PB",1125899906842624.0}};
static const UnitDef u_time[] = {
    {"s",1}, {"min",60}, {"hr",3600}, {"day",86400}, {"week",604800}, {"year",31557600}};

#define CNT(a) (int)(sizeof(a)/sizeof(a[0]))
static const Category cats[] = {
    {"Temperature", u_temp, CNT(u_temp), 1},
    {"Length",      u_len,  CNT(u_len),  0},
    {"Weight",      u_wt,   CNT(u_wt),   0},
    {"Area",        u_area, CNT(u_area), 0},
    {"Volume",      u_vol,  CNT(u_vol),  0},
    {"Speed",       u_spd,  CNT(u_spd),  0},
    {"Data",        u_data, CNT(u_data), 0},
    {"Time",        u_time, CNT(u_time), 0},
};

static double conv_temp(double v, int from, int to)
{
    // always go through celsius
    double c;
    switch (from) {
    case 0: c = v; break;
    case 1: c = (v - 32) * 5.0/9.0; break;
    case 2: c = v - 273.15; break;
    default: return NAN;
    }
    switch (to) {
    case 0: return c;
    case 1: return c * 9.0/5.0 + 32;
    case 2: return c + 273.15;
    }
    return NAN;
}

static double conv(const Category *cat, int from, int to, double v)
{
    if (cat->temp) return conv_temp(v, from, to);
    return v * cat->u[from].factor / cat->u[to].factor;
}

static int icasecmp(const char *a, const char *b)
{
    while (*a && *b) {
        if (tolower((unsigned char)*a) != tolower((unsigned char)*b)) return 0;
        a++; b++;
    }
    return *a == *b;
}

/* try to resolve a unit name (possibly with category prefix like "tempc") */
static int lookup_unit(const char *name, int *ci, int *ui)
{
    const char *s = name;
    if (*s == '-') s++;

    // direct match first
    for (int c = 0; c < NCATS; c++)
        for (int u = 0; u < cats[c].n; u++)
            if (icasecmp(cats[c].u[u].nm, s)) { *ci = c; *ui = u; return 1; }

    // try stripping known prefixes
    static const struct { const char *p; int c; } px[] = {
        {"temp",0},{"len",1},{"wt",2},{"weight",2},{"area",3},
        {"vol",4},{"spd",5},{"speed",5},{"data",6},{"time",7}};
    for (int i = 0; i < 10; i++) {
        int pl = strlen(px[i].p);
        if ((int)strlen(s) <= pl) continue;
        int ok = 1;
        for (int k = 0; k < pl; k++)
            if (tolower((unsigned char)s[k]) != px[i].p[k]) { ok = 0; break; }
        if (!ok) continue;
        int c = px[i].c;
        for (int u = 0; u < cats[c].n; u++)
            if (icasecmp(cats[c].u[u].nm, s + pl)) { *ci = c; *ui = u; return 1; }
    }
    return 0;
}

// ----------------------------------------------------------------
// CLI
// ----------------------------------------------------------------

static void usage(const char *me)
{
    printf("%s\n\nUsage: %s [OPTIONS]\n\n", VER, me);
    puts("  (no args)        Launch TUI\n");
    puts("Options:");
    puts("  -h, --help       Show help");
    puts("  --version        Version info");
    puts("  -c <cmd>         CLI mode\n");
    puts("Commands:");
    printf("  %s -c <expr>                   Evaluate expression\n", me);
    printf("  %s -c convert <val> <from> <to>  Convert units\n", me);
    printf("  %s -c convert -tempc 100 -tempf  (prefix style)\n", me);
    printf("  %s -c gcd <a> <b> [c ...]      GCD (PGCD)\n", me);
    printf("  %s -c lcm <a> <b> [c ...]      LCM (PPCM)\n", me);
    printf("  %s -c base <num> <from> <to>   Base conversion\n\n", me);
    puts("Units: C F K m km mi ft in cm mm yd kg g lb oz ton m2 km2 ft2");
    puts("       acre hectare L mL gal fl_oz cup m/s km/h mph knot");
    puts("       B KB MB GB TB PB s min hr day week year\n");
    puts("TUI: F1-F7/Tab switch modes | Enter eval | C clear | q quit");
}

static int cmd_eval(int ac, char **av, int i)
{
    char buf[MAXBUF] = "";
    for (; i < ac; i++) {
        if (i > 2) strncat(buf, " ", MAXBUF - strlen(buf) - 1);
        strncat(buf, av[i], MAXBUF - strlen(buf) - 1);
    }
    double r;
    if (eval(buf, 1, &r)) {
        char out[64]; fmtval(r, out, sizeof(out));
        puts(out);
        return 0;
    }
    fprintf(stderr, "Error: bad expression '%s'\n", buf);
    return 1;
}

static int cmd_conv(int ac, char **av, int from)
{
    double val = 0; int fc = -1, fu = -1, tc = -1, tu = -1, gotv = 0;
    for (int i = from; i < ac; i++) {
        int c, u;
        if (lookup_unit(av[i], &c, &u)) {
            if (fc < 0) { fc = c; fu = u; } else { tc = c; tu = u; }
        } else {
            char *end; double v = strtod(av[i], &end);
            if (end != av[i] && !*end) { val = v; gotv = 1; }
            else { fprintf(stderr, "Error: '%s'?\n", av[i]); return 1; }
        }
    }
    if (!gotv || fc < 0 || tc < 0) {
        fprintf(stderr, "Usage: calculator -c convert <val> <from> <to>\n"); return 1;
    }
    if (fc != tc) {
        fprintf(stderr, "Error: can't convert %s to %s\n", cats[fc].label, cats[tc].label);
        return 1;
    }
    double r = conv(&cats[fc], fu, tu, val);
    char out[64]; fmtval(r, out, sizeof(out));
    printf("%.10g %s = %s %s\n", val, cats[fc].u[fu].nm, out, cats[tc].u[tu].nm);
    return 0;
}

static int cmd_gcd(int ac, char **av, int from)
{
    if (ac - from < 2) { fprintf(stderr, "Need at least 2 numbers\n"); return 1; }
    long long r = atoll(av[from]);
    for (int i = from + 1; i < ac; i++) r = gcd(r, atoll(av[i]));
    printf("%lld\n", r);
    return 0;
}

static int cmd_lcm(int ac, char **av, int from)
{
    if (ac - from < 2) { fprintf(stderr, "Need at least 2 numbers\n"); return 1; }
    long long r = atoll(av[from]);
    for (int i = from + 1; i < ac; i++) r = lcm(r, atoll(av[i]));
    printf("%lld\n", r);
    return 0;
}

static int cmd_base(int ac, char **av, int from)
{
    if (ac - from < 3) {
        fprintf(stderr, "Usage: calculator -c base <num> <from> <to>\n"); return 1;
    }
    int fb = 10, tb = 10;
    if      (icasecmp(av[from+1], "dec")) fb = 10;
    else if (icasecmp(av[from+1], "hex")) fb = 16;
    else if (icasecmp(av[from+1], "oct")) fb = 8;
    else if (icasecmp(av[from+1], "bin")) fb = 2;
    else { fprintf(stderr, "Unknown base '%s'\n", av[from+1]); return 1; }

    if      (icasecmp(av[from+2], "dec")) tb = 10;
    else if (icasecmp(av[from+2], "hex")) tb = 16;
    else if (icasecmp(av[from+2], "oct")) tb = 8;
    else if (icasecmp(av[from+2], "bin")) tb = 2;
    else { fprintf(stderr, "Unknown base '%s'\n", av[from+2]); return 1; }

    char *end;
    long long v = strtoll(av[from], &end, fb);
    if (*end) { fprintf(stderr, "'%s' invalid for base %s\n", av[from], av[from+1]); return 1; }

    unsigned long long uv = v < 0 ? (unsigned long long)(-v) : (unsigned long long)v;
    switch (tb) {
    case 10: printf("%lld\n", v); break;
    case 16: printf("%s%llX\n", v < 0 ? "-" : "", uv); break;
    case 8:  printf("%s%llo\n", v < 0 ? "-" : "", uv); break;
    case 2:
        if (!v) { puts("0"); break; }
        { char bits[128]; int p = 0;
          unsigned long long tmp = uv;
          while (tmp && p < 120) { bits[p++] = '0' + (int)(tmp & 1); tmp >>= 1; }
          if (v < 0) putchar('-');
          for (int i = p-1; i >= 0; i--) putchar(bits[i]);
          putchar('\n');
        } break;
    }
    return 0;
}

static int handle_cli(int ac, char **av)
{
    for (int i = 1; i < ac; i++) {
        if (!strcmp(av[i], "--version")) { puts(VER); return 0; }
        if (!strcmp(av[i], "-h") || !strcmp(av[i], "--help")) { usage(av[0]); return 0; }
        if (!strcmp(av[i], "-c")) {
            if (++i >= ac) { fprintf(stderr, "-c needs an argument\n"); return 1; }
            if (icasecmp(av[i], "convert")) return cmd_conv(ac, av, i+1);
            if (icasecmp(av[i], "gcd"))     return cmd_gcd(ac, av, i+1);
            if (icasecmp(av[i], "lcm"))     return cmd_lcm(ac, av, i+1);
            if (icasecmp(av[i], "base"))    return cmd_base(ac, av, i+1);
            return cmd_eval(ac, av, i);
        }
    }
    usage(av[0]);
    return 1;
}

// ----------------------------------------------------------------
// TUI state
// ----------------------------------------------------------------

static int mode = M_CALC;
static int deg = 1; // 1=degrees 0=radians

static char input[MAXBUF]; static int inputlen;
static char hist_e[NHIST][MAXBUF], hist_r[NHIST][64]; static int nhist;

// conv mode
static int ccat, cfrom, cto, cfield;
static char cinput[MAXBUF]; static int cinputlen;

// base mode
enum { B_DEC, B_HEX, B_OCT, B_BIN, B_N };
static const char *bnames[] = {"DEC","HEX","OCT","BIN"};
static int bsel, bfield;
static char binput[MAXBUF]; static int binputlen;

// graph
static char gexpr[MAXBUF]; static int gexprlen;
static double gxmin = -10, gxmax = 10, gymin = -5, gymax = 5;
static int gediting = 1;

// matrix
enum { OP_DET, OP_T, OP_INV, OP_ADD, OP_MUL, NOP };
static const char *opnames[] = {"DET","TRANS","INV","ADD","MUL"};
static int mop, mfield;
static char ma_buf[MAXBUF], mb_buf[MAXBUF]; static int ma_len, mb_len;

// gcd mode
static char ginput[MAXBUF]; static int ginputlen;

// ----------------------------------------------------------------
// TUI helpers
// ----------------------------------------------------------------

static void hist_push(const char *e, const char *r)
{
    int i;
    if (nhist < NHIST) {
        i = nhist++;
    } else {
        for (int j = 0; j < NHIST-1; j++) {
            strcpy(hist_e[j], hist_e[j+1]);
            strcpy(hist_r[j], hist_r[j+1]);
        }
        i = NHIST-1;
    }
    snprintf(hist_e[i], MAXBUF, "%s", e);
    snprintf(hist_r[i], 64, "%s", r);
}

static void tabs(int w)
{
    const char *t[] = {
        " F1:Calc "," F2:Sci "," F3:Conv "," F4:Base ",
        " F5:Graph "," F6:Mat "," F7:GCD "
    };
    attron(A_BOLD);
    mvhline(0, 0, ' ', w);
    int x = 0;
    for (int i = 0; i < NMODES; i++) {
        if (i == mode) attron(A_REVERSE);
        mvprintw(0, x, "%s", t[i]);
        if (i == mode) attroff(A_REVERSE);
        x += strlen(t[i]);
    }
    attroff(A_BOLD);
    mvhline(1, 0, ACS_HLINE, w);
}

static void statusbar(int h, int w)
{
    mvhline(h-2, 0, ACS_HLINE, w);
    attron(A_DIM);
    switch (mode) {
    case M_CALC:
        mvprintw(h-1,1,"Enter:eval  C:clear  Bksp:del  F1-F7/Tab:mode  q:quit"); break;
    case M_SCI:
        mvprintw(h-1,1,"Enter:eval  d:deg/rad[%s]  C:clear  F1-F7/Tab:mode  q:quit",
                 deg?"DEG":"RAD"); break;
    case M_CONV:
        mvprintw(h-1,1,"Up/Dn:select  Tab:field  Enter:confirm  F1-F7:mode  q:quit"); break;
    case M_BASE:
        mvprintw(h-1,1,"Arrows:base  Tab:field  C:clear  F1-F7:mode  q:quit"); break;
    case M_GRAPH:
        mvprintw(h-1,1,"Enter:plot  Arrows:pan  +/-:zoom  C:clear  F1-F7:mode  q:quit"); break;
    case M_MAT:
        mvprintw(h-1,1,"Arrows:op  Tab:field  Enter:calc  C:clear  F1-F7:mode  q:quit"); break;
    case M_GCD:
        mvprintw(h-1,1,"Enter:calc  C:clear  F1-F7/Tab:mode  q:quit"); break;
    }
    attroff(A_DIM);
}

/* backspace helper */
static int is_bksp(int ch) { return ch == KEY_BACKSPACE || ch == 127 || ch == 8; }
static int is_enter(int ch) { return ch == '\n' || ch == '\r' || ch == KEY_ENTER; }

// ----------------------------------------------------------------
// mode handlers
// ----------------------------------------------------------------

static void show_calc(int h, int w)
{
    int y = 3;
    attron(A_DIM); mvprintw(y++, 2, "History:"); attroff(A_DIM);
    for (int i = 0; i < nhist && y+i < h-6; i++)
        mvprintw(y+i, 4, "%s = %s", hist_e[i], hist_r[i]);
    y = h-5;
    mvhline(y, 1, ACS_HLINE, w-2); y++;
    mvprintw(y, 2, "> ");
    attron(A_BOLD); mvprintw(y, 4, "%s", input); attroff(A_BOLD);
    mvprintw(y, 4+inputlen, "_");
}

static void on_calc(int ch)
{
    if (ch == 'C') { input[0] = 0; inputlen = 0; return; }
    if (is_bksp(ch)) {
        if (inputlen > 0) input[--inputlen] = 0;
        return;
    }
    if (is_enter(ch)) {
        if (!inputlen) return;
        double r; char out[64];
        if (eval(input, deg, &r)) fmtval(r, out, sizeof(out));
        else strcpy(out, "Error");
        hist_push(input, out);
        input[0] = 0; inputlen = 0;
        return;
    }
    if (inputlen < MAXBUF-1 && ch >= 32 && ch < 127) {
        input[inputlen++] = ch; input[inputlen] = 0;
    }
}

static void show_sci(int h, int w)
{
    int y = 3;
    attron(A_DIM); mvprintw(y++, 2, "History:"); attroff(A_DIM);
    for (int i = 0; i < nhist && y+i < h-14; i++)
        mvprintw(y+i, 4, "%s = %s", hist_e[i], hist_r[i]);

    int ry = h-13;
    if (ry < y+nhist) ry = y+nhist;
    attron(A_DIM); mvprintw(ry++, 2, "Functions:"); attroff(A_DIM);
    mvprintw(ry++, 4, "sin  cos  tan  asin  acos  atan");
    mvprintw(ry++, 4, "log  ln   exp  sqrt  cbrt  abs");
    mvprintw(ry++, 4, "ceil floor fact");
    ry++;
    mvprintw(ry++, 4, "Constants: pi, e");
    mvprintw(ry++, 4, "Angle: %s  (press 'd' to toggle)", deg ? "DEG" : "RAD");
    mvprintw(ry++, 4, "Example: sin(45) + sqrt(2^3+1)");

    y = h-5;
    mvhline(y, 1, ACS_HLINE, w-2); y++;
    mvprintw(y, 2, "> ");
    attron(A_BOLD); mvprintw(y, 4, "%s", input); attroff(A_BOLD);
    mvprintw(y, 4+inputlen, "_");
}

static void on_sci(int ch)
{
    if ((ch == 'd' || ch == 'D') && !inputlen) { deg = !deg; return; }
    on_calc(ch);
}

/* unit converter */
static void show_conv(int h, int w)
{
    (void)h;
    int y = 3;
    const Category *cat = &cats[ccat];

    attron(A_BOLD); mvprintw(y, 2, "Category:"); attroff(A_BOLD); y++;
    for (int i = 0; i < NCATS; i++) {
        if (cfield == 0 && i == ccat) attron(A_REVERSE);
        mvprintw(y+i, 4, "%-14s", cats[i].label);
        if (cfield == 0 && i == ccat) attroff(A_REVERSE);
    }

    int c2 = 22, c3 = 38;
    y = 3;
    attron(A_BOLD); mvprintw(y, c2, "From:"); mvprintw(y, c3, "To:"); attroff(A_BOLD);
    y++;
    for (int i = 0; i < cat->n; i++) {
        if (cfield == 1 && i == cfrom) attron(A_REVERSE);
        mvprintw(y+i, c2, "%-12s", cat->u[i].nm);
        if (cfield == 1 && i == cfrom) attroff(A_REVERSE);
        if (cfield == 2 && i == cto) attron(A_REVERSE);
        mvprintw(y+i, c3, "%-12s", cat->u[i].nm);
        if (cfield == 2 && i == cto) attroff(A_REVERSE);
    }

    int mu = cat->n > NCATS ? cat->n : NCATS;
    int iy = 4 + mu + 1;
    mvhline(iy, 1, ACS_HLINE, w-2); iy++;
    if (cfield == 3) attron(A_BOLD);
    mvprintw(iy, 2, "Value: %s_", cinput);
    if (cfield == 3) attroff(A_BOLD);
    iy += 2;
    if (cinputlen > 0) {
        double v = atof(cinput);
        double r = conv(cat, cfrom, cto, v);
        char out[64]; fmtval(r, out, sizeof(out));
        attron(A_BOLD | COLOR_PAIR(1));
        mvprintw(iy, 2, "%.10g %s = %s %s", v, cat->u[cfrom].nm, out, cat->u[cto].nm);
        attroff(A_BOLD | COLOR_PAIR(1));
    }
}

static void on_conv(int ch)
{
    const Category *cat = &cats[ccat];
    if (ch == '\t') { cfield = (cfield+1) % 4; return; }
    if (ch == KEY_UP || ch == KEY_DOWN) {
        int d = ch == KEY_UP ? -1 : 1;
        switch (cfield) {
        case 0: ccat = (ccat+d+NCATS)%NCATS; cfrom = 0;
                cto = cats[ccat].n > 1 ? 1 : 0; break;
        case 1: cfrom = (cfrom+d+cat->n)%cat->n; break;
        case 2: cto = (cto+d+cat->n)%cat->n; break;
        }
        return;
    }
    if (cfield == 3) {
        if (is_bksp(ch)) { if (cinputlen > 0) cinput[--cinputlen] = 0; return; }
        if ((isdigit(ch)||ch=='.'||ch=='-') && cinputlen < MAXBUF-1) {
            cinput[cinputlen++] = ch; cinput[cinputlen] = 0; return;
        }
    }
    if (is_enter(ch) && cfield < 3) cfield++;
}

/* base converter */
static long long parse_based(const char *s, int b)
{
    if (!s || !*s) return 0;
    char *end;
    int radix = (b == B_HEX) ? 16 : (b == B_OCT) ? 8 : (b == B_BIN) ? 2 : 10;
    return strtoll(s, &end, radix);
}

static void to_bin(long long v, char *buf, int sz)
{
    if (!v) { snprintf(buf, sz, "0"); return; }
    int neg = v < 0;
    unsigned long long u = neg ? (unsigned long long)(-v) : (unsigned long long)v;
    char tmp[128]; int p = 0;
    while (u && p < 120) { tmp[p++] = '0' + (u & 1); u >>= 1; }
    while (p % 4) tmp[p++] = '0'; // pad to nibble
    int bp = 0;
    if (neg) buf[bp++] = '-';
    for (int i = p-1; i >= 0 && bp < sz-2; i--) {
        buf[bp++] = tmp[i];
        if (i > 0 && !(i%4) && bp < sz-2) buf[bp++] = ' ';
    }
    buf[bp] = 0;
}

static void show_base(int h, int w)
{
    (void)h; (void)w;
    int y = 3;
    attron(A_BOLD); mvprintw(y, 2, "Base Converter"); attroff(A_BOLD);
    y += 2;
    mvprintw(y, 2, "Input base: ");
    for (int i = 0; i < B_N; i++) {
        if (!bfield && i == bsel) attron(A_REVERSE);
        mvprintw(y, 14+i*6, " %s ", bnames[i]);
        if (!bfield && i == bsel) attroff(A_REVERSE);
    }
    y += 2;
    if (bfield) attron(A_BOLD);
    mvprintw(y, 2, "Input (%s): %s_", bnames[bsel], binput);
    if (bfield) attroff(A_BOLD);
    y += 2;

    if (binputlen) {
        long long v = parse_based(binput, bsel);
        mvhline(y, 1, ACS_HLINE, w-2); y++;
        attron(A_BOLD); mvprintw(y++, 2, "Results:"); attroff(A_BOLD);
        char bb[256]; to_bin(v, bb, sizeof(bb));
        unsigned long long uv = v < 0 ? (unsigned long long)(-v) : (unsigned long long)v;
        attron(COLOR_PAIR(1));
        mvprintw(y++, 4, "DEC: %lld", v);
        mvprintw(y++, 4, "HEX: %s%llX", v<0?"-":"", uv);
        mvprintw(y++, 4, "OCT: %s%llo", v<0?"-":"", uv);
        mvprintw(y++, 4, "BIN: %s", bb);
        attroff(COLOR_PAIR(1));
        y++;
        attron(A_DIM);
        if (v >= 0 && v < 128)
            mvprintw(y++, 4, "ASCII: '%c'", (v >= 32 && v < 127) ? (char)v : '.');
        mvprintw(y++, 4, "Bits: %d",
                 !v ? 1 : (int)(log2(fabs((double)v))+1) + (v<0));
        attroff(A_DIM);
    } else {
        y++;
        attron(A_DIM);
        mvprintw(y++, 4, "Type a number to see it in all bases.");
        mvprintw(y++, 4, "DEC: 0-9   HEX: 0-9,a-f   OCT: 0-7   BIN: 0-1");
        attroff(A_DIM);
    }
}

static int valid_digit(int ch, int b)
{
    if (b == B_BIN) return ch == '0' || ch == '1';
    if (b == B_OCT) return ch >= '0' && ch <= '7';
    if (b == B_HEX) return isxdigit(ch);
    return isdigit(ch);
}

static void on_base(int ch)
{
    if (ch == '\t') { bfield ^= 1; return; }
    if (!bfield) {
        if (ch == KEY_LEFT || ch == KEY_UP) bsel = (bsel-1+B_N)%B_N;
        else if (ch == KEY_RIGHT || ch == KEY_DOWN) bsel = (bsel+1)%B_N;
        else if (is_enter(ch)) bfield = 1;
        return;
    }
    if (ch == 'C') { binput[0] = 0; binputlen = 0; return; }
    if (is_bksp(ch)) { if (binputlen) binput[--binputlen] = 0; return; }
    if (ch == '-' && !binputlen) {
        binput[0] = '-'; binput[1] = 0; binputlen = 1; return;
    }
    if (valid_digit(ch, bsel) && binputlen < MAXBUF-1) {
        binput[binputlen++] = toupper(ch); binput[binputlen] = 0;
    }
}

/* graph plotter */
static void autofit_y(void)
{
    if (!gexprlen) return;
    double lo = 1e30, hi = -1e30;
    double xr = gxmax - gxmin;
    int any = 0;
    for (int i = 0; i <= 200; i++) {
        double x = gxmin + (i / 200.0) * xr, y;
        if (!eval_at(gexpr, deg, x, &y) || isnan(y) || isinf(y)) continue;
        if (y < lo) lo = y;
        if (y > hi) hi = y;
        any = 1;
    }
    if (any) {
        double pad = (hi - lo) * 0.1;
        if (pad < 0.5) pad = 0.5;
        gymin = lo - pad; gymax = hi + pad;
    }
}

static void show_graph(int h, int w)
{
    int y = 3;
    attron(A_BOLD); mvprintw(y, 2, "f(x) = "); attroff(A_BOLD);
    if (gediting) attron(A_BOLD);
    mvprintw(y, 9, "%s_", gexpr);
    if (gediting) attroff(A_BOLD);
    y++;
    attron(A_DIM);
    mvprintw(y, 2, "x:[%.1f, %.1f]  y:[%.1f, %.1f]", gxmin, gxmax, gymin, gymax);
    attroff(A_DIM);
    y++; mvhline(y, 0, ACS_HLINE, w); y++;

    if (!gexprlen) {
        attron(A_DIM);
        mvprintw(y+2, 4, "Enter f(x) then press Enter to plot.");
        mvprintw(y+3, 4, "Examples: sin(x)  x^2  x^3-3*x");
        mvprintw(y+5, 4, "Arrows: pan    +/-: zoom    C: clear");
        attroff(A_DIM);
        return;
    }

    int ph = h-y-3, pw = w-4;
    if (ph < 3 || pw < 10) return;
    int top = y;
    double xr = gxmax - gxmin, yr = gymax - gymin;
    if (xr <= 0) xr = 1;
    if (yr <= 0) yr = 1;

    // axis positions
    int xaxis = -1, yaxis = -1;
    if (gymin <= 0 && gymax >= 0) xaxis = (int)((gymax/yr) * (ph-1));
    if (gxmin <= 0 && gxmax >= 0) yaxis = (int)((-gxmin/xr) * (pw-1));

    // draw x axis + ticks
    if (xaxis >= 0 && xaxis < ph) {
        attron(A_DIM);
        mvhline(top+xaxis, 2, ACS_HLINE, pw);
        double step = pow(10, floor(log10(xr/4)));
        for (double t = ceil(gxmin/step)*step; t <= gxmax; t += step) {
            int col = (int)(((t-gxmin)/xr)*(pw-1));
            if (col < 0 || col >= pw) continue;
            mvaddch(top+xaxis, 2+col, ACS_PLUS);
            char lb[16]; snprintf(lb, 16, "%.4g", t);
            int ll = strlen(lb);
            if (2+col-ll/2 >= 0 && 2+col+ll/2 < w && xaxis+1 < ph)
                mvprintw(top+xaxis+1, 2+col-ll/2, "%s", lb);
        }
        attroff(A_DIM);
    }
    // y axis
    if (yaxis >= 0 && yaxis < pw) {
        attron(A_DIM);
        mvvline(top, 2+yaxis, ACS_VLINE, ph);
        double step = pow(10, floor(log10(yr/4)));
        for (double t = ceil(gymin/step)*step; t <= gymax; t += step) {
            int row = (int)(((gymax-t)/yr)*(ph-1));
            if (row < 0 || row >= ph) continue;
            mvaddch(top+row, 2+yaxis, ACS_PLUS);
            char lb[16]; snprintf(lb, 16, "%.4g", t);
            if (yaxis >= 6) mvprintw(top+row, 2+yaxis-(int)strlen(lb)-1, "%s", lb);
            else            mvprintw(top+row, 2+yaxis+2, "%s", lb);
        }
        if (xaxis >= 0 && xaxis < ph) mvaddch(top+xaxis, 2+yaxis, ACS_PLUS);
        attroff(A_DIM);
    }

    // plot curve
    attron(A_BOLD | COLOR_PAIR(1));
    int prev = -1;
    for (int c = 0; c < pw; c++) {
        double x = gxmin + ((double)c/(pw-1)) * xr, yv;
        if (!eval_at(gexpr, deg, x, &yv) || isnan(yv) || isinf(yv)) { prev = -1; continue; }
        int row = (int)(((gymax - yv)/yr) * (ph-1));
        if (row >= 0 && row < ph) {
            mvaddch(top+row, 2+c, '*');
            // connect gaps vertically
            if (prev >= 0 && abs(row-prev) > 1) {
                int r1 = (prev<row ? prev+1 : row+1), r2 = (prev<row ? row-1 : prev-1);
                for (int r = r1; r <= r2 && r >= 0 && r < ph; r++)
                    mvaddch(top+r, 2+c, ':');
            }
        }
        prev = row;
    }
    attroff(A_BOLD | COLOR_PAIR(1));
}

static void on_graph(int ch)
{
    if (gediting) {
        if (ch == 'C') { gexpr[0] = 0; gexprlen = 0; return; }
        if (is_bksp(ch)) { if (gexprlen) gexpr[--gexprlen] = 0; return; }
        if (is_enter(ch)) {
            if (gexprlen) { gediting = 0; autofit_y(); }
            return;
        }
        if (gexprlen < MAXBUF-1 && ch >= 32 && ch < 127) {
            gexpr[gexprlen++] = ch; gexpr[gexprlen] = 0;
        }
        return;
    }

    // panning / zoom
    double xr = gxmax-gxmin, yr = gymax-gymin;
    double xc, yc;
    switch (ch) {
    case KEY_LEFT:  gxmin -= xr*.2; gxmax -= xr*.2; break;
    case KEY_RIGHT: gxmin += xr*.2; gxmax += xr*.2; break;
    case KEY_UP:    gymin += yr*.2; gymax += yr*.2; break;
    case KEY_DOWN:  gymin -= yr*.2; gymax -= yr*.2; break;
    case '+': case '=':
        xc = (gxmin+gxmax)/2; yc = (gymin+gymax)/2;
        xr *= .7; yr *= .7;
        gxmin = xc-xr/2; gxmax = xc+xr/2;
        gymin = yc-yr/2; gymax = yc+yr/2; break;
    case '-':
        xc = (gxmin+gxmax)/2; yc = (gymin+gymax)/2;
        xr *= 1.4; yr *= 1.4;
        gxmin = xc-xr/2; gxmax = xc+xr/2;
        gymin = yc-yr/2; gymax = yc+yr/2; break;
    case 'C': case 'c':
        gediting = 1; gexpr[0] = 0; gexprlen = 0;
        gxmin = -10; gxmax = 10; gymin = -5; gymax = 5; break;
    default:
        if (is_enter(ch)) gediting = 1;
        break;
    }
}

/* matrix mode */
static void show_mat(int h, int w)
{
    (void)h; (void)w;
    int y = 3;
    attron(A_BOLD); mvprintw(y, 2, "Matrix Calculator"); attroff(A_BOLD);
    y += 2;

    mvprintw(y, 2, "Op: ");
    for (int i = 0; i < NOP; i++) {
        if (!mfield && i == mop) attron(A_REVERSE);
        mvprintw(y, 6 + i*10, " %-8s", opnames[i]);
        if (!mfield && i == mop) attroff(A_REVERSE);
    }
    y += 2;

    int two = (mop == OP_ADD || mop == OP_MUL);

    if (mfield == 1) attron(A_BOLD);
    mvprintw(y++, 2, "A (;-separated): %s_", ma_buf);
    if (mfield == 1) attroff(A_BOLD);

    if (two) {
        if (mfield == 2) attron(A_BOLD);
        mvprintw(y++, 2, "B (;-separated): %s_", mb_buf);
        if (mfield == 2) attroff(A_BOLD);
    }
    y++;
    attron(A_DIM); mvprintw(y++, 4, "e.g. \"1 2; 3 4\" -> 2x2 matrix"); attroff(A_DIM);
    y++;

    Mat a, b, res;
    int ga = mat_parse(ma_buf, &a);
    if (ga) {
        attron(A_DIM); mvprintw(y++, 2, "A (%dx%d):", a.r, a.c); attroff(A_DIM);
        for (int i = 0; i < a.r; i++) {
            char rb[128]; fmtrow(&a, i, rb, sizeof(rb));
            mvprintw(y++, 4, "%s", rb);
        }
        y++;
    }
    int gb = 0;
    if (two && (gb = mat_parse(mb_buf, &b))) {
        attron(A_DIM); mvprintw(y++, 2, "B (%dx%d):", b.r, b.c); attroff(A_DIM);
        for (int i = 0; i < b.r; i++) {
            char rb[128]; fmtrow(&b, i, rb, sizeof(rb));
            mvprintw(y++, 4, "%s", rb);
        }
        y++;
    }
    if (!ga) return;

    mvhline(y, 1, ACS_HLINE, w-2); y++;
    attron(A_BOLD); mvprintw(y++, 2, "Result:"); attroff(A_BOLD);

    int ok;
    switch (mop) {
    case OP_DET:
        if (a.r != a.c) { mvprintw(y, 4, "Error: need square matrix"); break; }
        { char s[64]; fmtval(determinant(&a), s, sizeof(s));
          attron(COLOR_PAIR(1)); mvprintw(y, 4, "det = %s", s); attroff(COLOR_PAIR(1)); }
        break;
    case OP_T:
        transpose(&a, &res);
        attron(COLOR_PAIR(1));
        for (int i = 0; i < res.r; i++) { char rb[128]; fmtrow(&res, i, rb, 128); mvprintw(y++, 4, "%s", rb); }
        attroff(COLOR_PAIR(1));
        break;
    case OP_INV:
        if (a.r != a.c) { mvprintw(y, 4, "Error: need square matrix"); break; }
        if (!invert(&a, &res)) { mvprintw(y, 4, "Error: singular matrix"); break; }
        attron(COLOR_PAIR(1));
        for (int i = 0; i < res.r; i++) { char rb[128]; fmtrow(&res, i, rb, 128); mvprintw(y++, 4, "%s", rb); }
        attroff(COLOR_PAIR(1));
        break;
    case OP_ADD:
        if (!gb) { mvprintw(y, 4, "Enter matrix B"); break; }
        ok = mat_add(&a, &b, &res);
        if (!ok) { mvprintw(y, 4, "Error: dims don't match"); break; }
        attron(COLOR_PAIR(1));
        for (int i = 0; i < res.r; i++) { char rb[128]; fmtrow(&res, i, rb, 128); mvprintw(y++, 4, "%s", rb); }
        attroff(COLOR_PAIR(1));
        break;
    case OP_MUL:
        if (!gb) { mvprintw(y, 4, "Enter matrix B"); break; }
        ok = mat_mul(&a, &b, &res);
        if (!ok) { mvprintw(y, 4, "Error: A.cols != B.rows"); break; }
        attron(COLOR_PAIR(1));
        for (int i = 0; i < res.r; i++) { char rb[128]; fmtrow(&res, i, rb, 128); mvprintw(y++, 4, "%s", rb); }
        attroff(COLOR_PAIR(1));
        break;
    }
}

static void on_mat(int ch)
{
    int two = (mop == OP_ADD || mop == OP_MUL);
    int nf = two ? 3 : 2;
    if (ch == '\t') { mfield = (mfield+1) % nf; return; }

    if (!mfield) {
        if (ch == KEY_LEFT || ch == KEY_UP) mop = (mop-1+NOP)%NOP;
        else if (ch == KEY_RIGHT || ch == KEY_DOWN) mop = (mop+1)%NOP;
        else if (is_enter(ch)) mfield = 1;
        return;
    }

    char *buf = (mfield == 1) ? ma_buf : mb_buf;
    int  *len = (mfield == 1) ? &ma_len : &mb_len;

    if (ch == 'C') { buf[0] = 0; *len = 0; return; }
    if (is_bksp(ch)) { if (*len) buf[--(*len)] = 0; return; }
    if (*len < MAXBUF-1 && ch >= 32 && ch < 127) {
        buf[(*len)++] = ch; buf[*len] = 0;
    }
}

/* GCD/LCM (PGCD/PPCM) mode */
static void show_gcd(int h, int w)
{
    (void)w;
    int y = 3;
    attron(A_BOLD); mvprintw(y, 2, "GCD (PGCD) / LCM (PPCM)"); attroff(A_BOLD);
    y += 2;
    mvprintw(y, 2, "Numbers (comma-separated): ");
    attron(A_BOLD); mvprintw(y, 29, "%s_", ginput); attroff(A_BOLD);
    y += 2;

    if (!ginputlen) {
        attron(A_DIM);
        mvprintw(y++, 4, "Enter 2+ numbers: e.g. 12, 18  or  24, 36, 48");
        attroff(A_DIM);
        return;
    }

    // parse numbers
    long long nums[64]; int cnt = 0;
    { char tmp[MAXBUF]; snprintf(tmp, MAXBUF, "%s", ginput);
      char *p = tmp;
      while (*p && cnt < 64) {
          while (*p == ' ' || *p == ',') p++;
          if (!*p) break;
          char *end;
          long long v = strtoll(p, &end, 10);
          if (end == p) break;
          nums[cnt++] = v; p = end;
      }
    }
    if (cnt < 2) { attron(A_DIM); mvprintw(y, 4, "Need 2+ numbers"); attroff(A_DIM); return; }

    mvhline(y, 1, ACS_HLINE, w-2); y++;

    long long g = nums[0], l = nums[0];
    for (int i = 1; i < cnt; i++) { g = gcd(g, nums[i]); l = lcm(l, nums[i]); }

    attron(A_BOLD | COLOR_PAIR(1));
    mvprintw(y++, 4, "GCD (PGCD) = %lld", g);
    mvprintw(y++, 4, "LCM (PPCM) = %lld", l);
    attroff(A_BOLD | COLOR_PAIR(1));
    y++;

    attron(A_DIM); mvprintw(y++, 2, "Prime factorizations:"); attroff(A_DIM);
    for (int i = 0; i < cnt && y < h-4; i++) {
        long long n = nums[i] < 0 ? -nums[i] : nums[i];
        char fb[256]; factorize(n, fb, sizeof(fb));
        mvprintw(y++, 4, "%lld = %s", nums[i], fb);
    }
    y++;

    // euclidean algorithm trace for first pair
    if (cnt >= 2) {
        attron(A_DIM); mvprintw(y++, 2, "Euclid(%lld, %lld):", nums[0], nums[1]); attroff(A_DIM);
        long long a = nums[0] < 0 ? -nums[0] : nums[0];
        long long b = nums[1] < 0 ? -nums[1] : nums[1];
        int steps = 0;
        while (b && y < h-3 && steps++ < 20) {
            mvprintw(y++, 4, "%lld = %lld * %lld + %lld", a, a/b, b, a%b);
            long long t = b; b = a%b; a = t;
        }
    }
}

static void on_gcd(int ch)
{
    if (ch == 'C') { ginput[0] = 0; ginputlen = 0; return; }
    if (is_bksp(ch)) { if (ginputlen) ginput[--ginputlen] = 0; return; }
    if (ginputlen < MAXBUF-1 && (isdigit(ch) || ch == ',' || ch == ' ' || ch == '-')) {
        ginput[ginputlen++] = ch; ginput[ginputlen] = 0;
    }
}

// ----------------------------------------------------------------
// main
// ----------------------------------------------------------------

int main(int ac, char **av)
{
    if (ac > 1)
        return handle_cli(ac, av);

    setlocale(LC_ALL, "");
    initscr();
    cbreak(); noecho();
    keypad(stdscr, TRUE);
    curs_set(0);

    if (has_colors()) {
        start_color(); use_default_colors();
        init_pair(1, COLOR_GREEN, -1);
        init_pair(2, COLOR_CYAN, -1);
    }

    // zero out all the buffers
    input[0] = cinput[0] = binput[0] = gexpr[0] = 0;
    ma_buf[0] = mb_buf[0] = ginput[0] = 0;
    cto = 1; // default "to" unit

    for (;;) {
        int h, w;
        getmaxyx(stdscr, h, w);
        erase();
        tabs(w);
        statusbar(h, w);

        switch (mode) {
        case M_CALC:  show_calc(h, w); break;
        case M_SCI:   show_sci(h, w);  break;
        case M_CONV:  show_conv(h, w); break;
        case M_BASE:  show_base(h, w); break;
        case M_GRAPH: show_graph(h, w);break;
        case M_MAT:   show_mat(h, w);  break;
        case M_GCD:   show_gcd(h, w);  break;
        }
        refresh();

        int ch = getch();

        // F-keys switch mode no matter what
        if (ch >= KEY_F(1) && ch <= KEY_F(7)) { mode = ch - KEY_F(1); continue; }

        // ctrl-c always quits
        if (ch == 3) break;

        // q quits unless you're typing something
        if (ch == 'q' || ch == 'Q') {
            int typing = 0;
            switch (mode) {
            case M_CALC: case M_SCI: typing = inputlen > 0; break;
            case M_CONV: typing = (cfield == 3 && cinputlen); break;
            case M_BASE: typing = (bfield && binputlen); break;
            case M_GRAPH: typing = (gediting && gexprlen); break;
            case M_MAT: typing = (mfield >= 1 && ((mfield==1 && ma_len) || (mfield==2 && mb_len))); break;
            case M_GCD: typing = ginputlen > 0; break;
            }
            if (!typing) break;
        }

        // tab cycles mode (except where tab is used for fields)
        if (ch == '\t' && mode != M_CONV && mode != M_BASE && mode != M_MAT) {
            mode = (mode + 1) % NMODES;
            continue;
        }

        switch (mode) {
        case M_CALC:  on_calc(ch);  break;
        case M_SCI:   on_sci(ch);   break;
        case M_CONV:  on_conv(ch);  break;
        case M_BASE:  on_base(ch);  break;
        case M_GRAPH: on_graph(ch); break;
        case M_MAT:   on_mat(ch);   break;
        case M_GCD:   on_gcd(ch);   break;
        }
    }

    endwin();
    return 0;
}
