$title linear price model

* -----------------------------------------------------------------------------
* set options
* -----------------------------------------------------------------------------

* set datset option
$set ds cps_static_all_2022



* -----------------------------------------------------------------------------
* read in the static household dataset
* -----------------------------------------------------------------------------

set
    r       States,
    s       Goods and sectors from BEA,
    gm(s)   Margin related sectors,
    m       Margins (trade or transport),
    h       Household categories,
    trn     Transfer types;    

$gdxin 'household/datasets/WiNDC_%ds%.gdx'
$loaddc s r m gm h trn
alias(s,ss,g),(r,rr,q);

* Load year of parameters

parameter
    ys0(r,s,g)		Sectoral supply,
    id0(r,g,s)		Intermediate demand,
    ld0(r,s)		Labor demand,
    kd0(r,s)		Capital demand,
    ty0(r,s)		Output tax on production,
    m0(r,s)		Imports,
    x0(r,s)		Exports of goods and services,
    rx0(r,s)		Re-exports of goods and services,
    md0(r,m,s)		Total margin demand,
    nm0(r,g,m)		Margin demand from national market,
    dm0(r,g,m)		Margin supply from local market,
    s0(r,s)		Aggregate supply,
    a0(r,s)		Armington supply,
    ta0(r,s)		Tax net subsidy rate on intermediate demand,
    tm0(r,s)		Import tariff,
    yh0(r,s)		Household production,
    bopdef0(r)		Balance of payments,
    hhadj0(r)		Household adjustment,
    g0(r,s)		Government demand,
    i0(r,s)		Investment demand,
    xn0(r,g)		Regional supply to national market,
    xd0(r,g)		Regional supply to local market,
    dd0(r,g)		Regional demand from local  market,
    nd0(r,g)		Regional demand from national market,
    pop0(r,h)		Population (households or returns in millions),
    le0(r,q,h)		Household labor endowment,
    ke0(r,h)		Household interest payments,
    tk0(r)		Capital tax rate,
    tl_avg0(r,h)	Household average labor tax rate,
    tl0(r,h)		Household marginal labor tax rate,
    tfica0(r,h)		Household FICA labor tax rate,
    cd0(r,g,h)		Household level expenditures,
    c0(r,h)		Aggregate household level expenditures,
    sav0(r,h)		Household saving,
    fsav0		Foreign savings,
    fint0		Foreign interest payments,
    govdef0		Government deficit,
    taxrevL(r)		Tax revenue,
    taxrevK		Capital tax revenue,
    totsav0		Aggregate savings,
    trn0(r,h)		Household transfer payments,
    hhtrn0(r,h,trn)	Disaggregate transfer payments;

* production data:
$loaddc ys0 ld0 kd0 id0 ty0

* aggregate consumption data:
$loaddc yh0 cd0=cd0_h c0=c0_h i0 g0 bopdef0 hhadj0

* trade data:
$loaddc s0 xd0 xn0 x0 rx0 a0 nd0 dd0 m0 ta0 tm0

* margins:
$loaddc md0 nm0 dm0

* household data:
$loaddc le0 ke0 tk0 tl_avg0 tl0 tfica0 sav0 trn0 hhtrn0 pop0 fsav0 fint0
$gdxin



* -----------------------------------------------------------------------------
* check micro consistency of the dataset
* -----------------------------------------------------------------------------

parameters
    zp_y(r,s)      zero profit - production,
    zp_x(r,s)      zero profit - exports,
    zp_a(r,s)      zero profit - armington absorption,
    zp_ms(r,m)     zero profit - margins,
    zp_c(r,h)      zero profit - consumption,
    mkt_pa(r,s)    market clearance - armington trade,
    mkt_py(r,s)    market clearance - supply,
    mkt_pn(s)      market clearance - national market trade,
    mkt_pd(r,s)    market clearance - local market trade,
    mkt_pl(r)      market clearance - labor market,
    mkt_rk         market clearance - rental market,
    mkt_inv        market clearance - investment;

* zero profit:
zp_y(r,s) = round((1-ty0(r,s)) * sum(ss, ys0(r,s,ss)) -
    (ld0(r,s) + (1+tk0(r))*kd0(r,s) + sum(ss, id0(r,ss,s))),4);

zp_x(r,s) = round((x0(r,s)-rx0(r,s)) + xn0(r,s) + xd0(r,s) - s0(r,s),4);

zp_a(r,s) = round((1-ta0(r,s))*a0(r,s) + rx0(r,s) -
    (nd0(r,s) + dd0(r,s) + (1+tm0(r,s))*m0(r,s) + sum(m, md0(r,m,s))),4);

zp_ms(r,m) = round(sum(s, md0(r,m,s)) - (sum(s, nm0(r,s,m) + dm0(r,s,m))),4);

zp_c(r,h) = round(c0(r,h) - sum(s, cd0(r,s,h)),4);

* market clearance:
mkt_pa(r,s) = round(a0(r,s) -
    (sum(ss, id0(r,s,ss)) + sum(h, cd0(r,s,h)) + i0(r,s) + g0(r,s)),4);

mkt_py(r,s) = round(sum(ss, ys0(r,ss,s)) - s0(r,s),4);

mkt_pn(s) = round(sum(r, xn0(r,s) - (nd0(r,s) + sum(m, nm0(r,s,m)))),4);

mkt_pd(r,s) = round(xd0(r,s) - (dd0(r,s) + sum(m, dm0(r,s,m))),4);

mkt_pl(r) = round(sum((rr,h), le0(rr,r,h)) - sum(s, ld0(r,s)),4);

mkt_rk = round(sum((r,h), ke0(r,h)) - sum((r,s), kd0(r,s)),4);

mkt_inv = round(sum((r,h), sav0(r,h)) - sum((r,s), i0(r,s)),4);

parameter
    zp_report	zero profit report,
    mkt_report  market clearance report;

parameter zp_report;
zp_report(r,s,'y') = zp_y(r,s);
zp_report(r,s,'x') = zp_x(r,s);
zp_report(r,s,'a') = zp_a(r,s);
zp_report(r,m,'ms') = zp_ms(r,m);
zp_report(r,h,'c') = zp_c(r,h);
display zp_report;

mkt_report(r,s,'pa') = mkt_pa(r,s);
mkt_report(r,s,'py') = mkt_py(r,s);
mkt_report('usa',s,'pn') = mkt_pn(s);
mkt_report(r,s,'pd') = mkt_pd(r,s);
mkt_report(r,'labor','pl') = mkt_pl(r);
mkt_report('usa','capital','rk') = mkt_rk;
mkt_report('usa','invest','inv') = mkt_inv;
display mkt_report;



* -----------------------------------------------------------------------------
* linear price model
* -----------------------------------------------------------------------------

* initialize shock parameter

parameter
    cost0(r,s)     cost shock;

cost0(r,s) = 0;

* fixed quantity, endogenous price model

nonnegative
variables
    py(r,s)        output price (supply),
    pa(r,s)        armington composite price (demand),
    pd(r,s)        regional price,
    pn(s)          national price,
    px(r,s)        export price,
    pm(r,s)        import price,
    pmarg(r,m)     margin price,
    pl(r)          wage rate,
    rk             capital rental rate;

equations
    zpe_y(r,s)     zero profit: production,
    zpe_x(r,s)     zero profit: supply,
    zpe_a(r,s)     zero profit: armington composite,
    zpe_ms(r,m)    zero profit: margins,
    mkte_pn(s)     market clearance: national price;

zpe_y(r,s)$sum(ss, ys0(r,s,ss))..
    py(r,s)*(1-ty0(r,s))*sum(ss, ys0(r,s,ss)) =e=
      sum(ss, pa(r,ss)*id0(r,ss,s)) + pl(r)*ld0(r,s) + rk*(1+tk0(r))*kd0(r,s) +
      cost0(r,s);

zpe_x(r,s)$sum(ss, ys0(r,ss,s))..
    sum(ss, py(r,ss)*ys0(r,ss,s)) =e=
      px(r,s)*(x0(r,s)-rx0(r,s)) + pd(r,s)*(xn0(r,s) + xd0(r,s));

zpe_a(r,s)$a0(r,s)..
    (1-ta0(r,s))*pa(r,s)*a0(r,s) + px(r,s)*rx0(r,s)  =e=
      pd(r,s)*(nd0(r,s) + dd0(r,s)) + (1+tm0(r,s))*pm(r,s)*m0(r,s) +
      sum(m, pmarg(r,m)*md0(r,m,s));

zpe_ms(r,m)$sum(s, md0(r,m,s))..
    sum(s, pmarg(r,m)*md0(r,m,s)) =e= sum(s, pd(r,s)*(nm0(r,s,m) + dm0(r,s,m)));

mkte_pn(s)..
    pn(s)*sum(r, xd0(r,s)) =e= sum(r, py(r,s)*xd0(r,s));

model linear_price_model /zpe_y.py, zpe_x.pd, zpe_a.pa, zpe_ms.pmarg/;

* fix prices that are assumed to be exogenous to the model
pl.fx(r) = 1;
rk.fx = 1;
px.fx(r,s) = 1;
pm.fx(r,s) = 1;
pd.fx(r,s)$(not sum(ss, ys0(r,ss,s))) = 1;

* set level values
py.l(r,s) = 1;
pa.l(r,s) = 1;
pd.l(r,s) = 1;
pmarg.l(r,m) = 1;

linear_price_model.iterlim = 0;
solve linear_price_model using MCP;



* -----------------------------------------------------------------------------
* counterfactual
* -----------------------------------------------------------------------------

* run 1 billion dollar shock, distributed by production on utility sector
cost0(r,'uti') = 1 * sum(ss, ys0(r,'uti',ss)) / sum((ss,rr), ys0(rr,'uti',ss));

linear_price_model.iterlim = 1000;
solve linear_price_model using MCP;



* -----------------------------------------------------------------------------
* end
* -----------------------------------------------------------------------------
