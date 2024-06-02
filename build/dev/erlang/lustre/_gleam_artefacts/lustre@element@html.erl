-module(lustre@element@html).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([html/2, text/1, base/1, head/2, link/1, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, hgroup/2, main/2, nav/2, section/2, search/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, data/2, dfn/2, em/2, i/2, kbd/2, mark/2, q/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var/2, wbr/1, area/1, audio/2, img/1, map/2, track/1, video/2, embed/1, iframe/1, object/1, picture/2, portal/1, source/1, svg/2, math/2, canvas/1, noscript/2, script/2, del/2, ins/2, caption/2, col/1, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/2, details/2, dialog/2, summary/2, slot/1, template/2]).

-spec html(
    list(lustre@internals@vdom:attribute(PUG)),
    list(lustre@internals@vdom:element(PUG))
) -> lustre@internals@vdom:element(PUG).
html(Attrs, Children) ->
    lustre@element:element(<<"html"/utf8>>, Attrs, Children).

-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    lustre@element:text(Content).

-spec base(list(lustre@internals@vdom:attribute(PUO))) -> lustre@internals@vdom:element(PUO).
base(Attrs) ->
    lustre@element:element(<<"base"/utf8>>, Attrs, []).

-spec head(
    list(lustre@internals@vdom:attribute(PUS)),
    list(lustre@internals@vdom:element(PUS))
) -> lustre@internals@vdom:element(PUS).
head(Attrs, Children) ->
    lustre@element:element(<<"head"/utf8>>, Attrs, Children).

-spec link(list(lustre@internals@vdom:attribute(PUY))) -> lustre@internals@vdom:element(PUY).
link(Attrs) ->
    lustre@element:element(<<"link"/utf8>>, Attrs, []).

-spec meta(list(lustre@internals@vdom:attribute(PVC))) -> lustre@internals@vdom:element(PVC).
meta(Attrs) ->
    lustre@element:element(<<"meta"/utf8>>, Attrs, []).

-spec style(list(lustre@internals@vdom:attribute(PVG)), binary()) -> lustre@internals@vdom:element(PVG).
style(Attrs, Css) ->
    lustre@element:element(<<"style"/utf8>>, Attrs, [text(Css)]).

-spec title(list(lustre@internals@vdom:attribute(PVK)), binary()) -> lustre@internals@vdom:element(PVK).
title(Attrs, Content) ->
    lustre@element:element(<<"title"/utf8>>, Attrs, [text(Content)]).

-spec body(
    list(lustre@internals@vdom:attribute(PVO)),
    list(lustre@internals@vdom:element(PVO))
) -> lustre@internals@vdom:element(PVO).
body(Attrs, Children) ->
    lustre@element:element(<<"body"/utf8>>, Attrs, Children).

-spec address(
    list(lustre@internals@vdom:attribute(PVU)),
    list(lustre@internals@vdom:element(PVU))
) -> lustre@internals@vdom:element(PVU).
address(Attrs, Children) ->
    lustre@element:element(<<"address"/utf8>>, Attrs, Children).

-spec article(
    list(lustre@internals@vdom:attribute(PWA)),
    list(lustre@internals@vdom:element(PWA))
) -> lustre@internals@vdom:element(PWA).
article(Attrs, Children) ->
    lustre@element:element(<<"article"/utf8>>, Attrs, Children).

-spec aside(
    list(lustre@internals@vdom:attribute(PWG)),
    list(lustre@internals@vdom:element(PWG))
) -> lustre@internals@vdom:element(PWG).
aside(Attrs, Children) ->
    lustre@element:element(<<"aside"/utf8>>, Attrs, Children).

-spec footer(
    list(lustre@internals@vdom:attribute(PWM)),
    list(lustre@internals@vdom:element(PWM))
) -> lustre@internals@vdom:element(PWM).
footer(Attrs, Children) ->
    lustre@element:element(<<"footer"/utf8>>, Attrs, Children).

-spec header(
    list(lustre@internals@vdom:attribute(PWS)),
    list(lustre@internals@vdom:element(PWS))
) -> lustre@internals@vdom:element(PWS).
header(Attrs, Children) ->
    lustre@element:element(<<"header"/utf8>>, Attrs, Children).

-spec h1(
    list(lustre@internals@vdom:attribute(PWY)),
    list(lustre@internals@vdom:element(PWY))
) -> lustre@internals@vdom:element(PWY).
h1(Attrs, Children) ->
    lustre@element:element(<<"h1"/utf8>>, Attrs, Children).

-spec h2(
    list(lustre@internals@vdom:attribute(PXE)),
    list(lustre@internals@vdom:element(PXE))
) -> lustre@internals@vdom:element(PXE).
h2(Attrs, Children) ->
    lustre@element:element(<<"h2"/utf8>>, Attrs, Children).

-spec h3(
    list(lustre@internals@vdom:attribute(PXK)),
    list(lustre@internals@vdom:element(PXK))
) -> lustre@internals@vdom:element(PXK).
h3(Attrs, Children) ->
    lustre@element:element(<<"h3"/utf8>>, Attrs, Children).

-spec h4(
    list(lustre@internals@vdom:attribute(PXQ)),
    list(lustre@internals@vdom:element(PXQ))
) -> lustre@internals@vdom:element(PXQ).
h4(Attrs, Children) ->
    lustre@element:element(<<"h4"/utf8>>, Attrs, Children).

-spec h5(
    list(lustre@internals@vdom:attribute(PXW)),
    list(lustre@internals@vdom:element(PXW))
) -> lustre@internals@vdom:element(PXW).
h5(Attrs, Children) ->
    lustre@element:element(<<"h5"/utf8>>, Attrs, Children).

-spec h6(
    list(lustre@internals@vdom:attribute(PYC)),
    list(lustre@internals@vdom:element(PYC))
) -> lustre@internals@vdom:element(PYC).
h6(Attrs, Children) ->
    lustre@element:element(<<"h6"/utf8>>, Attrs, Children).

-spec hgroup(
    list(lustre@internals@vdom:attribute(PYI)),
    list(lustre@internals@vdom:element(PYI))
) -> lustre@internals@vdom:element(PYI).
hgroup(Attrs, Children) ->
    lustre@element:element(<<"hgroup"/utf8>>, Attrs, Children).

-spec main(
    list(lustre@internals@vdom:attribute(PYO)),
    list(lustre@internals@vdom:element(PYO))
) -> lustre@internals@vdom:element(PYO).
main(Attrs, Children) ->
    lustre@element:element(<<"main"/utf8>>, Attrs, Children).

-spec nav(
    list(lustre@internals@vdom:attribute(PYU)),
    list(lustre@internals@vdom:element(PYU))
) -> lustre@internals@vdom:element(PYU).
nav(Attrs, Children) ->
    lustre@element:element(<<"nav"/utf8>>, Attrs, Children).

-spec section(
    list(lustre@internals@vdom:attribute(PZA)),
    list(lustre@internals@vdom:element(PZA))
) -> lustre@internals@vdom:element(PZA).
section(Attrs, Children) ->
    lustre@element:element(<<"section"/utf8>>, Attrs, Children).

-spec search(
    list(lustre@internals@vdom:attribute(PZG)),
    list(lustre@internals@vdom:element(PZG))
) -> lustre@internals@vdom:element(PZG).
search(Attrs, Children) ->
    lustre@element:element(<<"search"/utf8>>, Attrs, Children).

-spec blockquote(
    list(lustre@internals@vdom:attribute(PZM)),
    list(lustre@internals@vdom:element(PZM))
) -> lustre@internals@vdom:element(PZM).
blockquote(Attrs, Children) ->
    lustre@element:element(<<"blockquote"/utf8>>, Attrs, Children).

-spec dd(
    list(lustre@internals@vdom:attribute(PZS)),
    list(lustre@internals@vdom:element(PZS))
) -> lustre@internals@vdom:element(PZS).
dd(Attrs, Children) ->
    lustre@element:element(<<"dd"/utf8>>, Attrs, Children).

-spec 'div'(
    list(lustre@internals@vdom:attribute(PZY)),
    list(lustre@internals@vdom:element(PZY))
) -> lustre@internals@vdom:element(PZY).
'div'(Attrs, Children) ->
    lustre@element:element(<<"div"/utf8>>, Attrs, Children).

-spec dl(
    list(lustre@internals@vdom:attribute(QAE)),
    list(lustre@internals@vdom:element(QAE))
) -> lustre@internals@vdom:element(QAE).
dl(Attrs, Children) ->
    lustre@element:element(<<"dl"/utf8>>, Attrs, Children).

-spec dt(
    list(lustre@internals@vdom:attribute(QAK)),
    list(lustre@internals@vdom:element(QAK))
) -> lustre@internals@vdom:element(QAK).
dt(Attrs, Children) ->
    lustre@element:element(<<"dt"/utf8>>, Attrs, Children).

-spec figcaption(
    list(lustre@internals@vdom:attribute(QAQ)),
    list(lustre@internals@vdom:element(QAQ))
) -> lustre@internals@vdom:element(QAQ).
figcaption(Attrs, Children) ->
    lustre@element:element(<<"figcaption"/utf8>>, Attrs, Children).

-spec figure(
    list(lustre@internals@vdom:attribute(QAW)),
    list(lustre@internals@vdom:element(QAW))
) -> lustre@internals@vdom:element(QAW).
figure(Attrs, Children) ->
    lustre@element:element(<<"figure"/utf8>>, Attrs, Children).

-spec hr(list(lustre@internals@vdom:attribute(QBC))) -> lustre@internals@vdom:element(QBC).
hr(Attrs) ->
    lustre@element:element(<<"hr"/utf8>>, Attrs, []).

-spec li(
    list(lustre@internals@vdom:attribute(QBG)),
    list(lustre@internals@vdom:element(QBG))
) -> lustre@internals@vdom:element(QBG).
li(Attrs, Children) ->
    lustre@element:element(<<"li"/utf8>>, Attrs, Children).

-spec menu(
    list(lustre@internals@vdom:attribute(QBM)),
    list(lustre@internals@vdom:element(QBM))
) -> lustre@internals@vdom:element(QBM).
menu(Attrs, Children) ->
    lustre@element:element(<<"menu"/utf8>>, Attrs, Children).

-spec ol(
    list(lustre@internals@vdom:attribute(QBS)),
    list(lustre@internals@vdom:element(QBS))
) -> lustre@internals@vdom:element(QBS).
ol(Attrs, Children) ->
    lustre@element:element(<<"ol"/utf8>>, Attrs, Children).

-spec p(
    list(lustre@internals@vdom:attribute(QBY)),
    list(lustre@internals@vdom:element(QBY))
) -> lustre@internals@vdom:element(QBY).
p(Attrs, Children) ->
    lustre@element:element(<<"p"/utf8>>, Attrs, Children).

-spec pre(
    list(lustre@internals@vdom:attribute(QCE)),
    list(lustre@internals@vdom:element(QCE))
) -> lustre@internals@vdom:element(QCE).
pre(Attrs, Children) ->
    lustre@element:element(<<"pre"/utf8>>, Attrs, Children).

-spec ul(
    list(lustre@internals@vdom:attribute(QCK)),
    list(lustre@internals@vdom:element(QCK))
) -> lustre@internals@vdom:element(QCK).
ul(Attrs, Children) ->
    lustre@element:element(<<"ul"/utf8>>, Attrs, Children).

-spec a(
    list(lustre@internals@vdom:attribute(QCQ)),
    list(lustre@internals@vdom:element(QCQ))
) -> lustre@internals@vdom:element(QCQ).
a(Attrs, Children) ->
    lustre@element:element(<<"a"/utf8>>, Attrs, Children).

-spec abbr(
    list(lustre@internals@vdom:attribute(QCW)),
    list(lustre@internals@vdom:element(QCW))
) -> lustre@internals@vdom:element(QCW).
abbr(Attrs, Children) ->
    lustre@element:element(<<"abbr"/utf8>>, Attrs, Children).

-spec b(
    list(lustre@internals@vdom:attribute(QDC)),
    list(lustre@internals@vdom:element(QDC))
) -> lustre@internals@vdom:element(QDC).
b(Attrs, Children) ->
    lustre@element:element(<<"b"/utf8>>, Attrs, Children).

-spec bdi(
    list(lustre@internals@vdom:attribute(QDI)),
    list(lustre@internals@vdom:element(QDI))
) -> lustre@internals@vdom:element(QDI).
bdi(Attrs, Children) ->
    lustre@element:element(<<"bdi"/utf8>>, Attrs, Children).

-spec bdo(
    list(lustre@internals@vdom:attribute(QDO)),
    list(lustre@internals@vdom:element(QDO))
) -> lustre@internals@vdom:element(QDO).
bdo(Attrs, Children) ->
    lustre@element:element(<<"bdo"/utf8>>, Attrs, Children).

-spec br(list(lustre@internals@vdom:attribute(QDU))) -> lustre@internals@vdom:element(QDU).
br(Attrs) ->
    lustre@element:element(<<"br"/utf8>>, Attrs, []).

-spec cite(
    list(lustre@internals@vdom:attribute(QDY)),
    list(lustre@internals@vdom:element(QDY))
) -> lustre@internals@vdom:element(QDY).
cite(Attrs, Children) ->
    lustre@element:element(<<"cite"/utf8>>, Attrs, Children).

-spec code(
    list(lustre@internals@vdom:attribute(QEE)),
    list(lustre@internals@vdom:element(QEE))
) -> lustre@internals@vdom:element(QEE).
code(Attrs, Children) ->
    lustre@element:element(<<"code"/utf8>>, Attrs, Children).

-spec data(
    list(lustre@internals@vdom:attribute(QEK)),
    list(lustre@internals@vdom:element(QEK))
) -> lustre@internals@vdom:element(QEK).
data(Attrs, Children) ->
    lustre@element:element(<<"data"/utf8>>, Attrs, Children).

-spec dfn(
    list(lustre@internals@vdom:attribute(QEQ)),
    list(lustre@internals@vdom:element(QEQ))
) -> lustre@internals@vdom:element(QEQ).
dfn(Attrs, Children) ->
    lustre@element:element(<<"dfn"/utf8>>, Attrs, Children).

-spec em(
    list(lustre@internals@vdom:attribute(QEW)),
    list(lustre@internals@vdom:element(QEW))
) -> lustre@internals@vdom:element(QEW).
em(Attrs, Children) ->
    lustre@element:element(<<"em"/utf8>>, Attrs, Children).

-spec i(
    list(lustre@internals@vdom:attribute(QFC)),
    list(lustre@internals@vdom:element(QFC))
) -> lustre@internals@vdom:element(QFC).
i(Attrs, Children) ->
    lustre@element:element(<<"i"/utf8>>, Attrs, Children).

-spec kbd(
    list(lustre@internals@vdom:attribute(QFI)),
    list(lustre@internals@vdom:element(QFI))
) -> lustre@internals@vdom:element(QFI).
kbd(Attrs, Children) ->
    lustre@element:element(<<"kbd"/utf8>>, Attrs, Children).

-spec mark(
    list(lustre@internals@vdom:attribute(QFO)),
    list(lustre@internals@vdom:element(QFO))
) -> lustre@internals@vdom:element(QFO).
mark(Attrs, Children) ->
    lustre@element:element(<<"mark"/utf8>>, Attrs, Children).

-spec q(
    list(lustre@internals@vdom:attribute(QFU)),
    list(lustre@internals@vdom:element(QFU))
) -> lustre@internals@vdom:element(QFU).
q(Attrs, Children) ->
    lustre@element:element(<<"q"/utf8>>, Attrs, Children).

-spec rp(
    list(lustre@internals@vdom:attribute(QGA)),
    list(lustre@internals@vdom:element(QGA))
) -> lustre@internals@vdom:element(QGA).
rp(Attrs, Children) ->
    lustre@element:element(<<"rp"/utf8>>, Attrs, Children).

-spec rt(
    list(lustre@internals@vdom:attribute(QGG)),
    list(lustre@internals@vdom:element(QGG))
) -> lustre@internals@vdom:element(QGG).
rt(Attrs, Children) ->
    lustre@element:element(<<"rt"/utf8>>, Attrs, Children).

-spec ruby(
    list(lustre@internals@vdom:attribute(QGM)),
    list(lustre@internals@vdom:element(QGM))
) -> lustre@internals@vdom:element(QGM).
ruby(Attrs, Children) ->
    lustre@element:element(<<"ruby"/utf8>>, Attrs, Children).

-spec s(
    list(lustre@internals@vdom:attribute(QGS)),
    list(lustre@internals@vdom:element(QGS))
) -> lustre@internals@vdom:element(QGS).
s(Attrs, Children) ->
    lustre@element:element(<<"s"/utf8>>, Attrs, Children).

-spec samp(
    list(lustre@internals@vdom:attribute(QGY)),
    list(lustre@internals@vdom:element(QGY))
) -> lustre@internals@vdom:element(QGY).
samp(Attrs, Children) ->
    lustre@element:element(<<"samp"/utf8>>, Attrs, Children).

-spec small(
    list(lustre@internals@vdom:attribute(QHE)),
    list(lustre@internals@vdom:element(QHE))
) -> lustre@internals@vdom:element(QHE).
small(Attrs, Children) ->
    lustre@element:element(<<"small"/utf8>>, Attrs, Children).

-spec span(
    list(lustre@internals@vdom:attribute(QHK)),
    list(lustre@internals@vdom:element(QHK))
) -> lustre@internals@vdom:element(QHK).
span(Attrs, Children) ->
    lustre@element:element(<<"span"/utf8>>, Attrs, Children).

-spec strong(
    list(lustre@internals@vdom:attribute(QHQ)),
    list(lustre@internals@vdom:element(QHQ))
) -> lustre@internals@vdom:element(QHQ).
strong(Attrs, Children) ->
    lustre@element:element(<<"strong"/utf8>>, Attrs, Children).

-spec sub(
    list(lustre@internals@vdom:attribute(QHW)),
    list(lustre@internals@vdom:element(QHW))
) -> lustre@internals@vdom:element(QHW).
sub(Attrs, Children) ->
    lustre@element:element(<<"sub"/utf8>>, Attrs, Children).

-spec sup(
    list(lustre@internals@vdom:attribute(QIC)),
    list(lustre@internals@vdom:element(QIC))
) -> lustre@internals@vdom:element(QIC).
sup(Attrs, Children) ->
    lustre@element:element(<<"sup"/utf8>>, Attrs, Children).

-spec time(
    list(lustre@internals@vdom:attribute(QII)),
    list(lustre@internals@vdom:element(QII))
) -> lustre@internals@vdom:element(QII).
time(Attrs, Children) ->
    lustre@element:element(<<"time"/utf8>>, Attrs, Children).

-spec u(
    list(lustre@internals@vdom:attribute(QIO)),
    list(lustre@internals@vdom:element(QIO))
) -> lustre@internals@vdom:element(QIO).
u(Attrs, Children) ->
    lustre@element:element(<<"u"/utf8>>, Attrs, Children).

-spec var(
    list(lustre@internals@vdom:attribute(QIU)),
    list(lustre@internals@vdom:element(QIU))
) -> lustre@internals@vdom:element(QIU).
var(Attrs, Children) ->
    lustre@element:element(<<"var"/utf8>>, Attrs, Children).

-spec wbr(list(lustre@internals@vdom:attribute(QJA))) -> lustre@internals@vdom:element(QJA).
wbr(Attrs) ->
    lustre@element:element(<<"wbr"/utf8>>, Attrs, []).

-spec area(list(lustre@internals@vdom:attribute(QJE))) -> lustre@internals@vdom:element(QJE).
area(Attrs) ->
    lustre@element:element(<<"area"/utf8>>, Attrs, []).

-spec audio(
    list(lustre@internals@vdom:attribute(QJI)),
    list(lustre@internals@vdom:element(QJI))
) -> lustre@internals@vdom:element(QJI).
audio(Attrs, Children) ->
    lustre@element:element(<<"audio"/utf8>>, Attrs, Children).

-spec img(list(lustre@internals@vdom:attribute(QJO))) -> lustre@internals@vdom:element(QJO).
img(Attrs) ->
    lustre@element:element(<<"img"/utf8>>, Attrs, []).

-spec map(
    list(lustre@internals@vdom:attribute(QJS)),
    list(lustre@internals@vdom:element(QJS))
) -> lustre@internals@vdom:element(QJS).
map(Attrs, Children) ->
    lustre@element:element(<<"map"/utf8>>, Attrs, Children).

-spec track(list(lustre@internals@vdom:attribute(QJY))) -> lustre@internals@vdom:element(QJY).
track(Attrs) ->
    lustre@element:element(<<"track"/utf8>>, Attrs, []).

-spec video(
    list(lustre@internals@vdom:attribute(QKC)),
    list(lustre@internals@vdom:element(QKC))
) -> lustre@internals@vdom:element(QKC).
video(Attrs, Children) ->
    lustre@element:element(<<"video"/utf8>>, Attrs, Children).

-spec embed(list(lustre@internals@vdom:attribute(QKI))) -> lustre@internals@vdom:element(QKI).
embed(Attrs) ->
    lustre@element:element(<<"embed"/utf8>>, Attrs, []).

-spec iframe(list(lustre@internals@vdom:attribute(QKM))) -> lustre@internals@vdom:element(QKM).
iframe(Attrs) ->
    lustre@element:element(<<"iframe"/utf8>>, Attrs, []).

-spec object(list(lustre@internals@vdom:attribute(QKQ))) -> lustre@internals@vdom:element(QKQ).
object(Attrs) ->
    lustre@element:element(<<"object"/utf8>>, Attrs, []).

-spec picture(
    list(lustre@internals@vdom:attribute(QKU)),
    list(lustre@internals@vdom:element(QKU))
) -> lustre@internals@vdom:element(QKU).
picture(Attrs, Children) ->
    lustre@element:element(<<"picture"/utf8>>, Attrs, Children).

-spec portal(list(lustre@internals@vdom:attribute(QLA))) -> lustre@internals@vdom:element(QLA).
portal(Attrs) ->
    lustre@element:element(<<"portal"/utf8>>, Attrs, []).

-spec source(list(lustre@internals@vdom:attribute(QLE))) -> lustre@internals@vdom:element(QLE).
source(Attrs) ->
    lustre@element:element(<<"source"/utf8>>, Attrs, []).

-spec svg(
    list(lustre@internals@vdom:attribute(QLI)),
    list(lustre@internals@vdom:element(QLI))
) -> lustre@internals@vdom:element(QLI).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-spec math(
    list(lustre@internals@vdom:attribute(QLO)),
    list(lustre@internals@vdom:element(QLO))
) -> lustre@internals@vdom:element(QLO).
math(Attrs, Children) ->
    lustre@element:element(<<"math"/utf8>>, Attrs, Children).

-spec canvas(list(lustre@internals@vdom:attribute(QLU))) -> lustre@internals@vdom:element(QLU).
canvas(Attrs) ->
    lustre@element:element(<<"canvas"/utf8>>, Attrs, []).

-spec noscript(
    list(lustre@internals@vdom:attribute(QLY)),
    list(lustre@internals@vdom:element(QLY))
) -> lustre@internals@vdom:element(QLY).
noscript(Attrs, Children) ->
    lustre@element:element(<<"noscript"/utf8>>, Attrs, Children).

-spec script(list(lustre@internals@vdom:attribute(QME)), binary()) -> lustre@internals@vdom:element(QME).
script(Attrs, Js) ->
    lustre@element:element(<<"script"/utf8>>, Attrs, [text(Js)]).

-spec del(
    list(lustre@internals@vdom:attribute(QMI)),
    list(lustre@internals@vdom:element(QMI))
) -> lustre@internals@vdom:element(QMI).
del(Attrs, Children) ->
    lustre@element:element(<<"del"/utf8>>, Attrs, Children).

-spec ins(
    list(lustre@internals@vdom:attribute(QMO)),
    list(lustre@internals@vdom:element(QMO))
) -> lustre@internals@vdom:element(QMO).
ins(Attrs, Children) ->
    lustre@element:element(<<"ins"/utf8>>, Attrs, Children).

-spec caption(
    list(lustre@internals@vdom:attribute(QMU)),
    list(lustre@internals@vdom:element(QMU))
) -> lustre@internals@vdom:element(QMU).
caption(Attrs, Children) ->
    lustre@element:element(<<"caption"/utf8>>, Attrs, Children).

-spec col(list(lustre@internals@vdom:attribute(QNA))) -> lustre@internals@vdom:element(QNA).
col(Attrs) ->
    lustre@element:element(<<"col"/utf8>>, Attrs, []).

-spec colgroup(
    list(lustre@internals@vdom:attribute(QNE)),
    list(lustre@internals@vdom:element(QNE))
) -> lustre@internals@vdom:element(QNE).
colgroup(Attrs, Children) ->
    lustre@element:element(<<"colgroup"/utf8>>, Attrs, Children).

-spec table(
    list(lustre@internals@vdom:attribute(QNK)),
    list(lustre@internals@vdom:element(QNK))
) -> lustre@internals@vdom:element(QNK).
table(Attrs, Children) ->
    lustre@element:element(<<"table"/utf8>>, Attrs, Children).

-spec tbody(
    list(lustre@internals@vdom:attribute(QNQ)),
    list(lustre@internals@vdom:element(QNQ))
) -> lustre@internals@vdom:element(QNQ).
tbody(Attrs, Children) ->
    lustre@element:element(<<"tbody"/utf8>>, Attrs, Children).

-spec td(
    list(lustre@internals@vdom:attribute(QNW)),
    list(lustre@internals@vdom:element(QNW))
) -> lustre@internals@vdom:element(QNW).
td(Attrs, Children) ->
    lustre@element:element(<<"td"/utf8>>, Attrs, Children).

-spec tfoot(
    list(lustre@internals@vdom:attribute(QOC)),
    list(lustre@internals@vdom:element(QOC))
) -> lustre@internals@vdom:element(QOC).
tfoot(Attrs, Children) ->
    lustre@element:element(<<"tfoot"/utf8>>, Attrs, Children).

-spec th(
    list(lustre@internals@vdom:attribute(QOI)),
    list(lustre@internals@vdom:element(QOI))
) -> lustre@internals@vdom:element(QOI).
th(Attrs, Children) ->
    lustre@element:element(<<"th"/utf8>>, Attrs, Children).

-spec thead(
    list(lustre@internals@vdom:attribute(QOO)),
    list(lustre@internals@vdom:element(QOO))
) -> lustre@internals@vdom:element(QOO).
thead(Attrs, Children) ->
    lustre@element:element(<<"thead"/utf8>>, Attrs, Children).

-spec tr(
    list(lustre@internals@vdom:attribute(QOU)),
    list(lustre@internals@vdom:element(QOU))
) -> lustre@internals@vdom:element(QOU).
tr(Attrs, Children) ->
    lustre@element:element(<<"tr"/utf8>>, Attrs, Children).

-spec button(
    list(lustre@internals@vdom:attribute(QPA)),
    list(lustre@internals@vdom:element(QPA))
) -> lustre@internals@vdom:element(QPA).
button(Attrs, Children) ->
    lustre@element:element(<<"button"/utf8>>, Attrs, Children).

-spec datalist(
    list(lustre@internals@vdom:attribute(QPG)),
    list(lustre@internals@vdom:element(QPG))
) -> lustre@internals@vdom:element(QPG).
datalist(Attrs, Children) ->
    lustre@element:element(<<"datalist"/utf8>>, Attrs, Children).

-spec fieldset(
    list(lustre@internals@vdom:attribute(QPM)),
    list(lustre@internals@vdom:element(QPM))
) -> lustre@internals@vdom:element(QPM).
fieldset(Attrs, Children) ->
    lustre@element:element(<<"fieldset"/utf8>>, Attrs, Children).

-spec form(
    list(lustre@internals@vdom:attribute(QPS)),
    list(lustre@internals@vdom:element(QPS))
) -> lustre@internals@vdom:element(QPS).
form(Attrs, Children) ->
    lustre@element:element(<<"form"/utf8>>, Attrs, Children).

-spec input(list(lustre@internals@vdom:attribute(QPY))) -> lustre@internals@vdom:element(QPY).
input(Attrs) ->
    lustre@element:element(<<"input"/utf8>>, Attrs, []).

-spec label(
    list(lustre@internals@vdom:attribute(QQC)),
    list(lustre@internals@vdom:element(QQC))
) -> lustre@internals@vdom:element(QQC).
label(Attrs, Children) ->
    lustre@element:element(<<"label"/utf8>>, Attrs, Children).

-spec legend(
    list(lustre@internals@vdom:attribute(QQI)),
    list(lustre@internals@vdom:element(QQI))
) -> lustre@internals@vdom:element(QQI).
legend(Attrs, Children) ->
    lustre@element:element(<<"legend"/utf8>>, Attrs, Children).

-spec meter(
    list(lustre@internals@vdom:attribute(QQO)),
    list(lustre@internals@vdom:element(QQO))
) -> lustre@internals@vdom:element(QQO).
meter(Attrs, Children) ->
    lustre@element:element(<<"meter"/utf8>>, Attrs, Children).

-spec optgroup(
    list(lustre@internals@vdom:attribute(QQU)),
    list(lustre@internals@vdom:element(QQU))
) -> lustre@internals@vdom:element(QQU).
optgroup(Attrs, Children) ->
    lustre@element:element(<<"optgroup"/utf8>>, Attrs, Children).

-spec option(list(lustre@internals@vdom:attribute(QRA)), binary()) -> lustre@internals@vdom:element(QRA).
option(Attrs, Label) ->
    lustre@element:element(
        <<"option"/utf8>>,
        Attrs,
        [lustre@element:text(Label)]
    ).

-spec output(
    list(lustre@internals@vdom:attribute(QRE)),
    list(lustre@internals@vdom:element(QRE))
) -> lustre@internals@vdom:element(QRE).
output(Attrs, Children) ->
    lustre@element:element(<<"output"/utf8>>, Attrs, Children).

-spec progress(
    list(lustre@internals@vdom:attribute(QRK)),
    list(lustre@internals@vdom:element(QRK))
) -> lustre@internals@vdom:element(QRK).
progress(Attrs, Children) ->
    lustre@element:element(<<"progress"/utf8>>, Attrs, Children).

-spec select(
    list(lustre@internals@vdom:attribute(QRQ)),
    list(lustre@internals@vdom:element(QRQ))
) -> lustre@internals@vdom:element(QRQ).
select(Attrs, Children) ->
    lustre@element:element(<<"select"/utf8>>, Attrs, Children).

-spec textarea(list(lustre@internals@vdom:attribute(QRW)), binary()) -> lustre@internals@vdom:element(QRW).
textarea(Attrs, Content) ->
    lustre@element:element(
        <<"textarea"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-spec details(
    list(lustre@internals@vdom:attribute(QSA)),
    list(lustre@internals@vdom:element(QSA))
) -> lustre@internals@vdom:element(QSA).
details(Attrs, Children) ->
    lustre@element:element(<<"details"/utf8>>, Attrs, Children).

-spec dialog(
    list(lustre@internals@vdom:attribute(QSG)),
    list(lustre@internals@vdom:element(QSG))
) -> lustre@internals@vdom:element(QSG).
dialog(Attrs, Children) ->
    lustre@element:element(<<"dialog"/utf8>>, Attrs, Children).

-spec summary(
    list(lustre@internals@vdom:attribute(QSM)),
    list(lustre@internals@vdom:element(QSM))
) -> lustre@internals@vdom:element(QSM).
summary(Attrs, Children) ->
    lustre@element:element(<<"summary"/utf8>>, Attrs, Children).

-spec slot(list(lustre@internals@vdom:attribute(QSS))) -> lustre@internals@vdom:element(QSS).
slot(Attrs) ->
    lustre@element:element(<<"slot"/utf8>>, Attrs, []).

-spec template(
    list(lustre@internals@vdom:attribute(QSW)),
    list(lustre@internals@vdom:element(QSW))
) -> lustre@internals@vdom:element(QSW).
template(Attrs, Children) ->
    lustre@element:element(<<"template"/utf8>>, Attrs, Children).
