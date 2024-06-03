-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-spec html(
    list(lustre@internals@vdom:attribute(RLL)),
    list(lustre@internals@vdom:element(RLL))
) -> lustre@internals@vdom:element(RLL).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-spec base(list(lustre@internals@vdom:attribute(RLT))) -> lustre@internals@vdom:element(RLT).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-spec head(
    list(lustre@internals@vdom:attribute(RLX)),
    list(lustre@internals@vdom:element(RLX))
) -> lustre@internals@vdom:element(RLX).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-spec link(list(lustre@internals@vdom:attribute(RMD))) -> lustre@internals@vdom:element(RMD).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-spec meta(list(lustre@internals@vdom:attribute(RMH))) -> lustre@internals@vdom:element(RMH).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-spec style(list(lustre@internals@vdom:attribute(RML)), binary()) -> lustre@internals@vdom:element(RML).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-spec title(list(lustre@internals@vdom:attribute(RMP)), binary()) -> lustre@internals@vdom:element(RMP).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-spec body(
    list(lustre@internals@vdom:attribute(RMT)),
    list(lustre@internals@vdom:element(RMT))
) -> lustre@internals@vdom:element(RMT).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-spec address(
    list(lustre@internals@vdom:attribute(RMZ)),
    list(lustre@internals@vdom:element(RMZ))
) -> lustre@internals@vdom:element(RMZ).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-spec article(
    list(lustre@internals@vdom:attribute(RNF)),
    list(lustre@internals@vdom:element(RNF))
) -> lustre@internals@vdom:element(RNF).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-spec aside(
    list(lustre@internals@vdom:attribute(RNL)),
    list(lustre@internals@vdom:element(RNL))
) -> lustre@internals@vdom:element(RNL).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-spec footer(
    list(lustre@internals@vdom:attribute(RNR)),
    list(lustre@internals@vdom:element(RNR))
) -> lustre@internals@vdom:element(RNR).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-spec header(
    list(lustre@internals@vdom:attribute(RNX)),
    list(lustre@internals@vdom:element(RNX))
) -> lustre@internals@vdom:element(RNX).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-spec h1(
    list(lustre@internals@vdom:attribute(ROD)),
    list(lustre@internals@vdom:element(ROD))
) -> lustre@internals@vdom:element(ROD).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-spec h2(
    list(lustre@internals@vdom:attribute(ROJ)),
    list(lustre@internals@vdom:element(ROJ))
) -> lustre@internals@vdom:element(ROJ).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-spec h3(
    list(lustre@internals@vdom:attribute(ROP)),
    list(lustre@internals@vdom:element(ROP))
) -> lustre@internals@vdom:element(ROP).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-spec h4(
    list(lustre@internals@vdom:attribute(ROV)),
    list(lustre@internals@vdom:element(ROV))
) -> lustre@internals@vdom:element(ROV).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-spec h5(
    list(lustre@internals@vdom:attribute(RPB)),
    list(lustre@internals@vdom:element(RPB))
) -> lustre@internals@vdom:element(RPB).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-spec h6(
    list(lustre@internals@vdom:attribute(RPH)),
    list(lustre@internals@vdom:element(RPH))
) -> lustre@internals@vdom:element(RPH).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-spec hgroup(
    list(lustre@internals@vdom:attribute(RPN)),
    list(lustre@internals@vdom:element(RPN))
) -> lustre@internals@vdom:element(RPN).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-spec main(
    list(lustre@internals@vdom:attribute(RPT)),
    list(lustre@internals@vdom:element(RPT))
) -> lustre@internals@vdom:element(RPT).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-spec nav(
    list(lustre@internals@vdom:attribute(RPZ)),
    list(lustre@internals@vdom:element(RPZ))
) -> lustre@internals@vdom:element(RPZ).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-spec section(
    list(lustre@internals@vdom:attribute(RQF)),
    list(lustre@internals@vdom:element(RQF))
) -> lustre@internals@vdom:element(RQF).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-spec search(
    list(lustre@internals@vdom:attribute(RQL)),
    list(lustre@internals@vdom:element(RQL))
) -> lustre@internals@vdom:element(RQL).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-spec blockquote(
    list(lustre@internals@vdom:attribute(RQR)),
    list(lustre@internals@vdom:element(RQR))
) -> lustre@internals@vdom:element(RQR).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-spec dd(
    list(lustre@internals@vdom:attribute(RQX)),
    list(lustre@internals@vdom:element(RQX))
) -> lustre@internals@vdom:element(RQX).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-spec 'div'(
    list(lustre@internals@vdom:attribute(RRD)),
    list(lustre@internals@vdom:element(RRD))
) -> lustre@internals@vdom:element(RRD).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-spec dl(
    list(lustre@internals@vdom:attribute(RRJ)),
    list(lustre@internals@vdom:element(RRJ))
) -> lustre@internals@vdom:element(RRJ).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-spec dt(
    list(lustre@internals@vdom:attribute(RRP)),
    list(lustre@internals@vdom:element(RRP))
) -> lustre@internals@vdom:element(RRP).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-spec figcaption(
    list(lustre@internals@vdom:attribute(RRV)),
    list(lustre@internals@vdom:element(RRV))
) -> lustre@internals@vdom:element(RRV).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-spec figure(
    list(lustre@internals@vdom:attribute(RSB)),
    list(lustre@internals@vdom:element(RSB))
) -> lustre@internals@vdom:element(RSB).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-spec hr(list(lustre@internals@vdom:attribute(RSH))) -> lustre@internals@vdom:element(RSH).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-spec li(
    list(lustre@internals@vdom:attribute(RSL)),
    list(lustre@internals@vdom:element(RSL))
) -> lustre@internals@vdom:element(RSL).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-spec menu(
    list(lustre@internals@vdom:attribute(RSR)),
    list(lustre@internals@vdom:element(RSR))
) -> lustre@internals@vdom:element(RSR).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-spec ol(
    list(lustre@internals@vdom:attribute(RSX)),
    list(lustre@internals@vdom:element(RSX))
) -> lustre@internals@vdom:element(RSX).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-spec p(
    list(lustre@internals@vdom:attribute(RTD)),
    list(lustre@internals@vdom:element(RTD))
) -> lustre@internals@vdom:element(RTD).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-spec pre(
    list(lustre@internals@vdom:attribute(RTJ)),
    list(lustre@internals@vdom:element(RTJ))
) -> lustre@internals@vdom:element(RTJ).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-spec ul(
    list(lustre@internals@vdom:attribute(RTP)),
    list(lustre@internals@vdom:element(RTP))
) -> lustre@internals@vdom:element(RTP).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-spec a(
    list(lustre@internals@vdom:attribute(RTV)),
    list(lustre@internals@vdom:element(RTV))
) -> lustre@internals@vdom:element(RTV).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-spec abbr(
    list(lustre@internals@vdom:attribute(RUB)),
    list(lustre@internals@vdom:element(RUB))
) -> lustre@internals@vdom:element(RUB).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-spec b(
    list(lustre@internals@vdom:attribute(RUH)),
    list(lustre@internals@vdom:element(RUH))
) -> lustre@internals@vdom:element(RUH).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-spec bdi(
    list(lustre@internals@vdom:attribute(RUN)),
    list(lustre@internals@vdom:element(RUN))
) -> lustre@internals@vdom:element(RUN).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-spec bdo(
    list(lustre@internals@vdom:attribute(RUT)),
    list(lustre@internals@vdom:element(RUT))
) -> lustre@internals@vdom:element(RUT).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-spec br(list(lustre@internals@vdom:attribute(RUZ))) -> lustre@internals@vdom:element(RUZ).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-spec cite(
    list(lustre@internals@vdom:attribute(RVD)),
    list(lustre@internals@vdom:element(RVD))
) -> lustre@internals@vdom:element(RVD).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-spec code(
    list(lustre@internals@vdom:attribute(RVJ)),
    list(lustre@internals@vdom:element(RVJ))
) -> lustre@internals@vdom:element(RVJ).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-spec data(
    list(lustre@internals@vdom:attribute(RVP)),
    list(lustre@internals@vdom:element(RVP))
) -> lustre@internals@vdom:element(RVP).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-spec dfn(
    list(lustre@internals@vdom:attribute(RVV)),
    list(lustre@internals@vdom:element(RVV))
) -> lustre@internals@vdom:element(RVV).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-spec em(
    list(lustre@internals@vdom:attribute(RWB)),
    list(lustre@internals@vdom:element(RWB))
) -> lustre@internals@vdom:element(RWB).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-spec i(
    list(lustre@internals@vdom:attribute(RWH)),
    list(lustre@internals@vdom:element(RWH))
) -> lustre@internals@vdom:element(RWH).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-spec kbd(
    list(lustre@internals@vdom:attribute(RWN)),
    list(lustre@internals@vdom:element(RWN))
) -> lustre@internals@vdom:element(RWN).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-spec mark(
    list(lustre@internals@vdom:attribute(RWT)),
    list(lustre@internals@vdom:element(RWT))
) -> lustre@internals@vdom:element(RWT).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-spec q(
    list(lustre@internals@vdom:attribute(RWZ)),
    list(lustre@internals@vdom:element(RWZ))
) -> lustre@internals@vdom:element(RWZ).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-spec rp(
    list(lustre@internals@vdom:attribute(RXF)),
    list(lustre@internals@vdom:element(RXF))
) -> lustre@internals@vdom:element(RXF).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-spec rt(
    list(lustre@internals@vdom:attribute(RXL)),
    list(lustre@internals@vdom:element(RXL))
) -> lustre@internals@vdom:element(RXL).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-spec ruby(
    list(lustre@internals@vdom:attribute(RXR)),
    list(lustre@internals@vdom:element(RXR))
) -> lustre@internals@vdom:element(RXR).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-spec s(
    list(lustre@internals@vdom:attribute(RXX)),
    list(lustre@internals@vdom:element(RXX))
) -> lustre@internals@vdom:element(RXX).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-spec samp(
    list(lustre@internals@vdom:attribute(RYD)),
    list(lustre@internals@vdom:element(RYD))
) -> lustre@internals@vdom:element(RYD).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-spec small(
    list(lustre@internals@vdom:attribute(RYJ)),
    list(lustre@internals@vdom:element(RYJ))
) -> lustre@internals@vdom:element(RYJ).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-spec span(
    list(lustre@internals@vdom:attribute(RYP)),
    list(lustre@internals@vdom:element(RYP))
) -> lustre@internals@vdom:element(RYP).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-spec strong(
    list(lustre@internals@vdom:attribute(RYV)),
    list(lustre@internals@vdom:element(RYV))
) -> lustre@internals@vdom:element(RYV).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-spec sub(
    list(lustre@internals@vdom:attribute(RZB)),
    list(lustre@internals@vdom:element(RZB))
) -> lustre@internals@vdom:element(RZB).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-spec sup(
    list(lustre@internals@vdom:attribute(RZH)),
    list(lustre@internals@vdom:element(RZH))
) -> lustre@internals@vdom:element(RZH).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-spec time(
    list(lustre@internals@vdom:attribute(RZN)),
    list(lustre@internals@vdom:element(RZN))
) -> lustre@internals@vdom:element(RZN).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-spec u(
    list(lustre@internals@vdom:attribute(RZT)),
    list(lustre@internals@vdom:element(RZT))
) -> lustre@internals@vdom:element(RZT).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-spec var(
    list(lustre@internals@vdom:attribute(RZZ)),
    list(lustre@internals@vdom:element(RZZ))
) -> lustre@internals@vdom:element(RZZ).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-spec wbr(list(lustre@internals@vdom:attribute(SAF))) -> lustre@internals@vdom:element(SAF).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-spec area(list(lustre@internals@vdom:attribute(SAJ))) -> lustre@internals@vdom:element(SAJ).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-spec audio(
    list(lustre@internals@vdom:attribute(SAN)),
    list(lustre@internals@vdom:element(SAN))
) -> lustre@internals@vdom:element(SAN).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-spec img(list(lustre@internals@vdom:attribute(SAT))) -> lustre@internals@vdom:element(SAT).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-spec map(
    list(lustre@internals@vdom:attribute(SAX)),
    list(lustre@internals@vdom:element(SAX))
) -> lustre@internals@vdom:element(SAX).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-spec track(list(lustre@internals@vdom:attribute(SBD))) -> lustre@internals@vdom:element(SBD).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-spec video(
    list(lustre@internals@vdom:attribute(SBH)),
    list(lustre@internals@vdom:element(SBH))
) -> lustre@internals@vdom:element(SBH).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-spec embed(list(lustre@internals@vdom:attribute(SBN))) -> lustre@internals@vdom:element(SBN).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-spec iframe(list(lustre@internals@vdom:attribute(SBR))) -> lustre@internals@vdom:element(SBR).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-spec object(list(lustre@internals@vdom:attribute(SBV))) -> lustre@internals@vdom:element(SBV).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-spec picture(
    list(lustre@internals@vdom:attribute(SBZ)),
    list(lustre@internals@vdom:element(SBZ))
) -> lustre@internals@vdom:element(SBZ).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-spec portal(list(lustre@internals@vdom:attribute(SCF))) -> lustre@internals@vdom:element(SCF).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-spec source(list(lustre@internals@vdom:attribute(SCJ))) -> lustre@internals@vdom:element(SCJ).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-spec svg(
    list(lustre@internals@vdom:attribute(SCN)),
    list(lustre@internals@vdom:element(SCN))
) -> lustre@internals@vdom:element(SCN).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec math(
    list(lustre@internals@vdom:attribute(SCT)),
    list(lustre@internals@vdom:element(SCT))
) -> lustre@internals@vdom:element(SCT).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-spec canvas(list(lustre@internals@vdom:attribute(SCZ))) -> lustre@internals@vdom:element(SCZ).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-spec noscript(
    list(lustre@internals@vdom:attribute(SDD)),
    list(lustre@internals@vdom:element(SDD))
) -> lustre@internals@vdom:element(SDD).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-spec script(list(lustre@internals@vdom:attribute(SDJ)), binary()) -> lustre@internals@vdom:element(SDJ).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-spec del(
    list(lustre@internals@vdom:attribute(SDN)),
    list(lustre@internals@vdom:element(SDN))
) -> lustre@internals@vdom:element(SDN).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-spec ins(
    list(lustre@internals@vdom:attribute(SDT)),
    list(lustre@internals@vdom:element(SDT))
) -> lustre@internals@vdom:element(SDT).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-spec caption(
    list(lustre@internals@vdom:attribute(SDZ)),
    list(lustre@internals@vdom:element(SDZ))
) -> lustre@internals@vdom:element(SDZ).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-spec col(list(lustre@internals@vdom:attribute(SEF))) -> lustre@internals@vdom:element(SEF).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-spec colgroup(
    list(lustre@internals@vdom:attribute(SEJ)),
    list(lustre@internals@vdom:element(SEJ))
) -> lustre@internals@vdom:element(SEJ).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-spec table(
    list(lustre@internals@vdom:attribute(SEP)),
    list(lustre@internals@vdom:element(SEP))
) -> lustre@internals@vdom:element(SEP).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-spec tbody(
    list(lustre@internals@vdom:attribute(SEV)),
    list(lustre@internals@vdom:element(SEV))
) -> lustre@internals@vdom:element(SEV).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-spec td(
    list(lustre@internals@vdom:attribute(SFB)),
    list(lustre@internals@vdom:element(SFB))
) -> lustre@internals@vdom:element(SFB).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-spec tfoot(
    list(lustre@internals@vdom:attribute(SFH)),
    list(lustre@internals@vdom:element(SFH))
) -> lustre@internals@vdom:element(SFH).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-spec th(
    list(lustre@internals@vdom:attribute(SFN)),
    list(lustre@internals@vdom:element(SFN))
) -> lustre@internals@vdom:element(SFN).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-spec thead(
    list(lustre@internals@vdom:attribute(SFT)),
    list(lustre@internals@vdom:element(SFT))
) -> lustre@internals@vdom:element(SFT).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-spec tr(
    list(lustre@internals@vdom:attribute(SFZ)),
    list(lustre@internals@vdom:element(SFZ))
) -> lustre@internals@vdom:element(SFZ).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-spec button(
    list(lustre@internals@vdom:attribute(SGF)),
    list(lustre@internals@vdom:element(SGF))
) -> lustre@internals@vdom:element(SGF).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-spec datalist(
    list(lustre@internals@vdom:attribute(SGL)),
    list(lustre@internals@vdom:element(SGL))
) -> lustre@internals@vdom:element(SGL).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-spec fieldset(
    list(lustre@internals@vdom:attribute(SGR)),
    list(lustre@internals@vdom:element(SGR))
) -> lustre@internals@vdom:element(SGR).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-spec form(
    list(lustre@internals@vdom:attribute(SGX)),
    list(lustre@internals@vdom:element(SGX))
) -> lustre@internals@vdom:element(SGX).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-spec input(list(lustre@internals@vdom:attribute(SHD))) -> lustre@internals@vdom:element(SHD).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-spec label(
    list(lustre@internals@vdom:attribute(SHH)),
    list(lustre@internals@vdom:element(SHH))
) -> lustre@internals@vdom:element(SHH).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-spec legend(
    list(lustre@internals@vdom:attribute(SHN)),
    list(lustre@internals@vdom:element(SHN))
) -> lustre@internals@vdom:element(SHN).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-spec meter(
    list(lustre@internals@vdom:attribute(SHT)),
    list(lustre@internals@vdom:element(SHT))
) -> lustre@internals@vdom:element(SHT).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-spec optgroup(
    list(lustre@internals@vdom:attribute(SHZ)),
    list(lustre@internals@vdom:element(SHZ))
) -> lustre@internals@vdom:element(SHZ).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-spec option(list(lustre@internals@vdom:attribute(SIF)), binary()) -> lustre@internals@vdom:element(SIF).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-spec output(
    list(lustre@internals@vdom:attribute(SIJ)),
    list(lustre@internals@vdom:element(SIJ))
) -> lustre@internals@vdom:element(SIJ).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-spec progress(
    list(lustre@internals@vdom:attribute(SIP)),
    list(lustre@internals@vdom:element(SIP))
) -> lustre@internals@vdom:element(SIP).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-spec select(
    list(lustre@internals@vdom:attribute(SIV)),
    list(lustre@internals@vdom:element(SIV))
) -> lustre@internals@vdom:element(SIV).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-spec textarea(list(lustre@internals@vdom:attribute(SJB)), binary()) -> lustre@internals@vdom:element(SJB).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec details(
    list(lustre@internals@vdom:attribute(SJF)),
    list(lustre@internals@vdom:element(SJF))
) -> lustre@internals@vdom:element(SJF).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-spec dialog(
    list(lustre@internals@vdom:attribute(SJL)),
    list(lustre@internals@vdom:element(SJL))
) -> lustre@internals@vdom:element(SJL).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-spec summary(
    list(lustre@internals@vdom:attribute(SJR)),
    list(lustre@internals@vdom:element(SJR))
) -> lustre@internals@vdom:element(SJR).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-spec slot(list(lustre@internals@vdom:attribute(SJX))) -> lustre@internals@vdom:element(SJX).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-spec template(
    list(lustre@internals@vdom:attribute(SKB)),
    list(lustre@internals@vdom:element(SKB))
) -> lustre@internals@vdom:element(SKB).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
