implement Qwm;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
	draw: Draw;
	Wmcontext, Context, Display, Image, Screen, Point, Pointer, Rect, Font: import draw;
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "devpointer.m";
	devpointer: Devpointer;
include "sh.m";
	sh: Sh;

Qwm: module {
	init:	fn(nil: ref Context, args: list of string);
};

Modchar: con 'q';
Mod: con -16r60;	# against lower case only
Program: con "wm/run";
Winmin: con 16;	# desired min height of window
Colmin: con 50;	# desired min width of column
Tagcolor: con Draw->Greyblue;
Tagselcolor: con Draw->Palegreyblue;

dflag := 0;
tagcmds: list of ref (string, string);
fontname: string;

Mstack, Msingle: con iota;	# Col.mode
modes := array[] of {"stack", "single"};

Wwant, Wlayout: con 1<<iota;	# for layout()
Skbd, Sptr, Scontrol: con 1<<iota;	# Win.started
start := array[] of {"kbd", "ptr", "control"};
Win: adt {
	index:	int;
	colindex:	int;
	tag:	string;		# given on wmctl, requested through Draw->Context
	fid:	int;		# of wmctl
	wm:	ref Wmcontext;
	nbwm:	ref Wmcontext;	# non-blocking wm, has different kbd,ptr,ctl,images chans that don't block
	img:	ref Image;	# nil initially
	started:	int;
	pids:	list of int;	# buffer pids and such
	wantr:	Rect;		# desired rect for img (according to mode)
	layoutr:	Rect;	# desired rect for img as if mode is Mlayout
	haver:	Rect;		# current rect for img (as sent or being sent to app), taking screen/window origin into account
	resizing:	int;	# whether !size msg sent or will be sent soon
	fixedorigin:	int;
};
wingen := 1;	# tag is "w"+wingen

Col: adt {
	index:	int;
	visindex:	int;
	r:	Rect;			# excluding tagr
	fr:	Rect;			# including tagr
	win:	ref Win;		# current win
	wins:	array of ref Win;	# all wins in col
	tagr:	Rect;			# desired tagr
	tag:	ref Image;		# may be nil, or not yet match tagr
	mode:	int;
	tagwins:	array of Rect;
	tagcmds:	array of Rect;
	moder:	Rect;
};

Cfg: adt {
	c:	array of ref Col;
	w:	array of int;
	col:	ref Col;
	tag:	string;
};

cols,				# all columns, non-visible have width 0
vis: array of ref Col;		# visible cols only, in order
col: ref Col;			# col with focus (never nil).  col.win may be nil.
tagsrect: Rect;			# space that holds the tag bars, at top
tagptr: ref Pointer;		# non-nil when mouse was pressed on tag
otherwin: string;		# tag of "other win", either previous with focus, or last unhidden
othercfg: ref Cfg;		# previous column configuration
prevptr: ref Pointer;

# currently moving (dragging) with pointer
moving: ref (ref Pointer, ref Win, chan of (int, string), array of byte, Point);

wctlc: chan of (ref Win, string);	# demux wmcontext wctl chans
kbdc: chan of int;

drawctxt: ref Context;
ptrfd: ref Sys->FD;
B1, B2, B3: con 1<<iota;

tagfont: ref Font;
fd2: ref Sys->FD;
zeropt: Point;
zerorect: Rect;

init(ctxt: ref Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	draw = load Draw Draw->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	devpointer = load Devpointer Devpointer->PATH;
	devpointer->init();
	sh = load Sh Sh->PATH;

	arg->init(args);
	arg->setusage(arg->progname()+" [-d] [-f font] [-t tag cmd] [profile]");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	dflag++;
		'f' =>	fontname = arg->arg();
		't' =>
			tag := arg->arg();
			cmd := arg->arg();
			tagcmds = ref (tag, cmd)::tagcmds;
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args > 1)
		arg->usage();
	tagcmds = rev(tagcmds);
	if(args != nil)
		profile := hd args;

	if(ctxt != nil)
		fail("already have draw context");

	sys->bind("#s", "/chan", Sys->MBEFORE);
	fiorect := sys->file2chan("/chan", "wmrect");
	if(fiorect == nil)
		fail(sprint("/chan/wmrect: %r"));
	fioctl := sys->file2chan("/chan", "wmctl");
	if(fioctl == nil)
		fail(sprint("/chan/wmctl: %r"));

	disp := Display.allocate(nil);
	if(disp == nil)
		fail(sprint("disp.allocate: %r"));
	scr := Screen.allocate(disp.image, disp.color(draw->Grey), 0);
	if(scr == nil)
		fail(sprint("screen.allocate: %r"));
	scr.image.draw(scr.image.r, scr.fill, disp.opaque, zeropt);

	wmc := chan of (string, chan of (string, ref Wmcontext));
	drawctxt = ref Context(disp, scr, wmc);

	cols = array[9] of ref Col;
	if(fontname != nil)
		tagfont = Font.open(disp, fontname);
	if(tagfont == nil)
		tagfont = Font.open(disp, "*default*");
	tagsrect = scr.image.r;
	tagsrect.max.y = tagsrect.min.y+tagfont.height+2;
	for(i := 0; i < len cols; i++) {
		fr := r := scr.image.r;
		tagr := tagsrect;
		if(i != 0)
			fr.max.x = r.max.x = tagr.max.x = r.min.x;
		r.min.y = tagr.max.y;
		cols[i] = ref Col(i, -1, r, fr, nil, nil, tagr, nil, Mstack, nil, nil, zerorect);
	}
	vis = array[] of {cols[0]};
	cols[0].visindex = 0;
	col = cols[0];
	drawtags();

	cmdc := chan of int;
	kbdc = chan of int;
	ptrc := chan of ref Pointer;
	wctlc = chan of (ref Win, string);

	spawn kbd(cmdc, pidc := chan of (int, string));
	(nil, kerr) := <-pidc;
	if(kerr != nil)
		fail("kbd: "+kerr);

	ptrfd = sys->open("/dev/pointer", Sys->ORDWR);
	if(ptrfd == nil)
		fail(sprint("open /dev/pointer: %r"));
	spawn ptr(ptrc, ppidc := chan of int);
	<-ppidc;

	if(profile != nil)
		spawn run(list of {"sh", "-n", profile});
	prevptr = ref Pointer;
	prevptr.xy = scr.image.r.min;

	for(;;)
	alt {
	(w, s) := <-wctlc =>
		say(sprint("wctl from %q: %q", w.tag, s));
		# xxx which programs use this?

	x := <-cmdc =>
		key(x);
		drawtags();

	x := <-kbdc =>
		if(col.win != nil && col.win.started&Skbd)
			alt {
			col.win.nbwm.kbd <-= x =>
				{}
			* =>
				{} # key lost, no big deal
			}
	p := <-ptrc =>
		mouse(p);
		prevptr = p;

	(tag, rc) := <-drawctxt.wm =>
		say("wmc: "+tag);
		(nil, w) := winfindtag(tag);
		if(w == nil)
			err := sprint("no window %#q", tag);
		else
			# give clone, at least necessary that we don't hold on to connfd,
			# the fileio to our wmctl.
			wm := ref *w.wm;
		spawn sendwm(rc, (err, wm));

	(off, count, nil, rc) := <-fiorect.read =>
		if(rc != nil) {
			buf := array of byte r2s(drawctxt.screen.image.r);
			s := min(len buf, off);
			e := min(len buf, off+count);
			rc <-= (buf[s:e], nil);
		}

	(nil, buf, nil, wc) := <-fiorect.write =>
		if(wc != nil) {
			say(sprint("fiorect.write buf %q", string buf));
			wc <-= (-1, "no writes");
		}

	(off, count, fid, rc) := <-fioctl.read =>
		if(rc != nil) {
			w := winfindfid(fid);
			if(w == nil) {
				w = winmk(fid);
				drawtags();
			}
			buf := array of byte w.tag;
			s := min(len buf, off);
			e := min(len buf, off+count);
			say(sprint("fioctl.read, returning %q", string buf[s:e]));
			rc <-= (buf[s:e], nil);
		}

	(nil, buf, fid, wc) := <-fioctl.write =>
		if(wc == nil) {
			w := winfindfid(fid);
			say(sprint("win %s has gone", w.tag));
			killall(w.pids);
			hadfocus := col.win==w;
			windel(cols[w.colindex], w);
			focus(col, col.win, 1);
			if(hadfocus && col.win != nil)
				winkbd(col.win, 1);
			resize();
		} else {
			w := winfindfid(fid);
			if(w == nil)
				wc <-= (-1, sprint("no window for fid %d", fid));
			else
				{
					ctlwrite(w, buf, wc);
				} exception ex {
				"error:*" =>
					ex = ex[len "error:":];
					say(sprint("fio.ctlwrite: %s", ex));
					wc <-= (-1, ex);
				}
		}
		drawtags();
	}
}

wctlfwd(w: ref Win, pidc: chan of int)
{
	pidc <-= pid();
	for(;;)
		wctlc <-= (w, <-w.wm.wctl);
}


List: adt[T] {
	first,
	last:	ref Link[T];

	empty:	fn(l: self ref List): int;
	take:	fn(l: self ref List): T;
	add:	fn(l: self ref List, e: T);
};

Link: adt[T] {
	e:	T;
	next:	cyclic ref Link[T];
};

List[T].empty(l: self ref List): int
{
	return l.first == nil;
}

List[T].take(l: self ref List): T
{
	e := l.first.e;
	if(l.first == l.last)
		l.first = l.last = nil;
	else
		l.first = l.first.next;
	return e;
}

List[T].add(l: self ref List, e: T)
{
	nl := ref Link[T](e, nil);
	if(l.first == nil)
		l.first = l.last = nl;
	else {
		l.last.next = nl;
		l.last = l.last.next;
	}
}


# buffer T's from 'fc' to 'tc', so we can also send on fc
# when sending to tc would block.
chanbuf[T](fc, tc: chan of T, pidc: chan of int)
{
	pidc <-= pid();
	l := ref List[T];
	e: T;
	bogusc := chan of T;
	cc := bogusc;
	for(;;)
	alt {
	cc <-= e =>
		if(!l.empty())
			e = l.take();
		else {
			e = nil;
			cc = bogusc;
		}
	ne := <-fc =>
		if(e == nil) {
			e = ne;
			cc = tc;
		} else
			l.add(ne);
	}
}


ptr(ptrc: chan of ref Pointer, pidc: chan of int)
{
	pidc <-= pid();
	buf := array[Devpointer->Size] of byte;
	for(;;) {
		n := sys->read(ptrfd, buf, len buf);
		if(n != len buf)
			return warn(sprint("ptr read gave %d", n));
		ptrc <-= devpointer->bytes2ptr(buf);
	}
}

ptrbox(c: ref Col)
{
	b := colbox(c);
	ptrset(b.min.add((b.dx()/2, b.dy()/2)));
}

ptrset(xy: Point)
{
	p := ref Pointer;
	p.xy = xy;
	buf := devpointer->ptr2bytes(p);
	n := sys->write(ptrfd, buf, len buf);
	if(n != len buf)
		warn(sprint("write pointer: %r"));
}

kbd(cmdc: chan of int, pidc: chan of (int, string))
{
	b := bufio->open("/dev/keyboard", Sys->OREAD);
	if(b == nil) {
		pidc <-= (-1, sprint("/dev/keyboard: %r"));
		return;
	}
	pidc <-= (pid(), nil);

	mod := 0;
	for(;;)
	case c := b.getc() {
	bufio->EOF =>
		return warn("eof on /dev/keyboard");
	bufio->ERROR =>
		return warn(sprint("error on /dev/keyboard: %r"));
	* =>
		if(mod) {
			if(c == Modchar)
				kbdc <-= Mod+Modchar;
			else
				cmdc <-= c;
			mod = 0;
		} else if(c == Mod+Modchar)
			mod = 1;
		else
			kbdc <-= c;
	}
}

keysend(v: int)
{
	kbdc <-= v;
}


run(argv: list of string)
{
	sys->pctl(Sys->NEWPGRP, nil);
	sh->run(drawctxt, argv);
}

run0()
{
	sys->pctl(Sys->FORKNS|Sys->NEWPGRP, nil);
	mod := load Command sprint("/dis/%s.dis", Program);
	if(mod == nil)
		return warn(sprint("load: %r"));
	spawn mod->init(drawctxt, list of {Program});
}


mouse(p: ref Pointer)
{
	if(moving != nil) {
		if(p.buttons == 0) {
			ptrmoving(p);
			drawtags();
		}
		return;
	}
	if(tagptr != nil || tagsrect.contains(p.xy))
		return ptrtag(p);

	if(!col.r.contains(p.xy) || (col.win != nil && !col.win.wantr.contains(p.xy))) {
		(c, w) := winfindpt(p.xy);
		if(c == nil)
			return;
		focus(c, w, 0);
		drawtags();
	}
	if(col.win != nil && col.win.started&Sptr)
		col.win.nbwm.ptr <-= p;
}

ptrmoving(p: ref Pointer)
{
	(optr, w, wc, buf, pt) := *moving;
	moving = nil;
	c := cols[w.colindex];
	(oc, ow) := winfindpt(p.xy);
	if(oc == nil) {
		# nothing
	} else if(abs(p.xy.x-pt.x) < 10 && abs(p.xy.y-pt.y) < 10) {
		if(c.mode == Mstack)
			case optr.buttons {
			B1 =>	winbigger(c, w, pt, 1, c.r.dy()/6);
			B2 =>	winmax(c, w);
			B3 =>	winsingle(c, w);
			}
	} else if(c != oc) {
		winmovecol(w, c, oc, p.xy.y-tagsrect.dy());
	} else if(w == ow || (ow != nil && ow.index+1 == w.index)) {
		if(w.index != 0) {
			if(w == ow)
				ow = c.wins[w.index-1];
			# move top of win up or down
			dy := p.xy.y-pt.y;
			ow.layoutr.max.y += dy;
			ow.wantr.max.y += dy;
			w.layoutr.min.y += dy;
			w.wantr.min.y += dy;
			resize();
		}
	} else {
		# move past other win
		oh := getheights(c);
		nh: array of int;
		(c.wins, nh) = movepast(c.wins, oh, w.index, ow.index, p.xy.y-tagsrect.dy());
		renumber(c.wins);
		setheights(c, nh);
		resize();
	}
	err := reshape(w);
	if(err != nil)
		error(err);
	wc <-= (len buf, nil);
	w.nbwm.images <-= w.img;
}

ptrtag(p: ref Pointer)
{
	if(tagptr == nil) {
		# first buttons down, or just movement
		if(p.buttons)
			tagptr = ref *p;
		else if(!col.fr.contains(p.xy)) {
			(c, w) := winfindpt(p.xy);
			if(w == nil && len c.wins != 0)
				w = c.wins[0];
			focus(c, w, 0);
			drawtags();
		}
		return;
	}
	if((tagptr.buttons^p.buttons) == 0) {
		# no buttons changed
		return;
	}
	if(p.buttons & ~tagptr.buttons) {
		# more buttons pressed, mark as cancelled
		tagptr.buttons = ~0;
		return;
	}
	if(p.buttons == 0 && tagptr.buttons == ~0) {
		# buttons up, but this was cancelled
		tagptr = nil;
		return;
	}

	say(sprint("release of %d at %d,%d", tagptr.buttons, tagptr.xy.x, tagptr.xy.y));
	(x, nil) := winfindpt(tagptr.xy);
	(y, nil) := winfindpt(p.xy);
	if(y == nil) {
		# nothing
	} else if(colbox(x).contains(tagptr.xy)) {
		if(x == y || x.visindex == y.visindex+1) {
			if(colbox(x).contains(p.xy))
				case tagptr.buttons {
				B1 =>	colbigger(x);
				B2 =>	colmax(x);
				B3 =>	colsingle(x);
				}
			else {
				colsetleft(x, p.xy.x);
				ptrbox(x);
			}
		} else if(tagptr.buttons == B1) {
			ow := getviswidths();
			(nc, nw) := movepast(vis, ow, x.visindex, y.visindex, p.xy.x);
			visset(nc, nw);
		}
	} else if(x.moder.contains(tagptr.xy)) {
		case tagptr.buttons {
		B1 =>
			if(x.mode == Mstack || x.mode == Msingle)
				modetoggle();
		B3 =>
			coltoggle(x);
		}
	} else if((j := rectindex(x.tagwins, tagptr.xy)) >= 0) {
		w := x.wins[j];
		if(x == y) {
			if(col != x || x.win != w || x.mode == Msingle)
				focus(x, w, 0);
			else
				case tagptr.buttons {
				B1 =>	winbigger(x, w, zeropt, 0, x.r.dy()/6);
				B2 =>	winmax(x, w);
				B3 =>	winsingle(x, w);
				}
		} else if(tagptr.buttons == B1) {
			winmovecol(w, x, y, p.xy.y-tagsrect.dy());
		}
	} else if((j = rectindex(x.tagcmds, tagptr.xy)) >= 0 && tagptr.buttons == B2) {
		(nil, cmd) := *l2a(tagcmds)[j];
		spawn run(list of {"sh", "-nc", cmd});
	} 
	tagptr = nil;
	drawtags();
}

key(x: int)
{
	say(sprint("cmd %c", x));
	if(x < 16r20)
		x -= Mod;
	case x {
	* =>
		# shift 1-9
		ops := array[] of {"!", "@", "#", "$", "%", "^", "&", "*", "("};
		ci := findstr(ops, sprint("%c", x));
		if(ci >= 0) {
			cfg := cfgget();
			coltoggle(cols[ci]);
			cfgset(cfg);
		}
	Modchar =>
		cfg := cfgget();
		colswitch();
		cfgset(cfg);
	'0' =>
		cfg := cfgget();
		colnonempty();
		cfgset(cfg);
		ptrensure(col, col.win);
	'1' to '9' =>
		cfg := cfgget();
		colsingle(cols[x-'1']);
		cfgset(cfg);
		ptrensure(col, col.win);
	'f' =>
		modetoggle();
	'F' =>
		cfg := cfgget();
		colsingle(col);
		cfgset(cfg);
		modesingle();
	'h' or
	'l' =>
		c := coladj(x=='l');
		if(c != nil)
			focus(c, c.win, 1);
	'H' or
	'L' =>
		c := coladj(x=='L');
		if(c != nil) {
			winmovecol(col.win, col, c, prevptr.xy.y-tagsrect.dy());
			ptrensure(col, col.win);
		}
	'K' or
	'J' =>
		w := winadj(x=='J');
		if(col.mode != Mstack || w == nil)
			return;
		h := getheights(col);
		cwi := col.win.index;
		(h[w.index], h[cwi]) = (h[cwi], h[w.index]);
		(col.wins[w.index], col.wins[cwi]) = (col.wins[cwi], col.wins[w.index]);
		renumber(col.wins);
		setheights(col, h);
		resize();
		ptrensure(col, col.win);
	'k' or
	'j' =>
		w := winadj(x=='j');
		if(w != nil)
			focus(col, w, 1);
	'<' =>
		i := col.visindex;
		if(i == 0)
			return;
		want := drawctxt.screen.image.r.dx()/8;
		w := getviswidths();
		take := scavenge(want, Colmin, 0, i, 1, w);
		if(take != 0) {
			w[i] += take;
			setviswidths(w);
			resize();
			ptrensure(col, col.win);
		}
	'.' =>
		i := col.visindex;
		if(i == len vis-1)
			return;
		want := drawctxt.screen.image.r.dx()/8;
		w := getviswidths();
		take := scavenge(want, Colmin, i+1, len vis, 1, w);
		if(take != 0) {
			w[i] += take;
			setviswidths(w);
			resize();
			ptrensure(col, col.win);
		}
	'>' =>
		want := drawctxt.screen.image.r.dx()/8;
		take := min(want, max(0, col.r.dx()-Colmin));
		i := col.visindex;
		if(i-1 < 0 || take == 0)
			return;
		w := getviswidths();
		w[i] -= take;
		w[i-1] += take;
		setviswidths(w);
		resize();
		ptrensure(col, col.win);
	',' =>
		want := drawctxt.screen.image.r.dx()/8;
		take := min(want, max(0, col.r.dx()-Colmin));
		i := col.visindex;
		if(i+1 >= len vis || take == 0)
			return;
		w := getviswidths();
		w[i] -= take;
		w[i+1] += take;
		setviswidths(w);
		resize();
		ptrensure(col, col.win);
	'u' =>
		if(col.mode == Mstack && col.win != nil)
			winmax(col, col.win);
	'i' =>
		if(col.mode == Mstack && col.win != nil) {
			winbigger(col, col.win, zeropt, 0, col.r.dy()/4);
			ptrensure(col, col.win);
		}
	'o' or 'm' or 'O' or 'M' =>
		if(col.mode != Mstack || col.win == nil && len col.wins <= 1)
			return;

		wi := col.win.index;
		if(x == 'o' && wi > 0)
			condense(col, wi-1, -1, wi, 0);
		else if(x == 'm' && wi > 0)
			condense(col, wi, wi+1, wi-1, Winmin);
		else if(x == 'O' && wi < len col.wins-1)
			condense(col, wi, wi+1, wi+1, Winmin);
		else if(x == 'M' && wi < len col.wins-1)
			condense(col, wi+1, len col.wins, wi, 0);
		else
			return;
		ptrensure(col, col.win);
	'X' =>
		if(col.win != nil) {
			# xxx not good if application was about to receive an image
			killall(col.win.pids);
			spawn send(col.win.wm.ctl, "exit");
		}
	'c' =>
		spawn run0();
	'w' =>
		(c, w) := winfindtag(otherwin);
		if(w == nil)
			return;
		if(c.visindex < 0) {
			cfg := cfgget();
			coltoggle(c);
			cfgset(cfg);
		}
		focus(c, w, 1);
	}
}

winadj(after: int): ref Win
{
	w := col.win;
	if(w == nil)
		return nil;
	i := w.index-1;
	if(after)
		i = w.index+1;
	if(i >= 0 && i < len col.wins)
		return col.wins[i];
	return nil;
}

# return column adjacent to 'col', on right side (after), or left side (!after)
coladj(after: int): ref Col
{
	ni := col.visindex-1;
	if(after)
		ni = col.visindex+1;
	if(ni >= 0 && ni < len vis)
		return vis[ni];
	return nil;
}

sendwm(rc: chan of (string, ref Wmcontext), t: (string, ref Wmcontext))
{
	rc <-= t;
}

send[T](c: chan of T, e: T)
{
	c <-= e;
}

winctl(w: ref Win, s: string)
{
	say(sprint("ctl to tag %s, win.index %d: %s", w.tag, w.index, s));
	w.nbwm.ctl <-= s;
}

winkbd(w: ref Win, v: int)
{
	winctl(w, sprint("haskbdfocus %d", v));
}


# for use in ctlwrite
error(s: string)
{
	raise "error:"+s;
}

narg(cmd: string, want, have: int)
{
	if(want != have)
		raise sprint("error:%#q needs %d args, saw %d", cmd, want, have);
}

ctlwrite(w: ref Win, buf: array of byte, wc: chan of (int, string))
{
	s := string buf;
	say(sprint("ctlwrite %q: %s", w.tag, s));
	a := l2a(str->unquoted(s));
	if(len a == 0)
		error("bad request");
	cmd := a[0];
	a = a[1:];
	doimg := str->prefix("!", cmd);
	case cmd {
	"!reshape" =>
		# !reshape tag reqid minx miny maxx maxy [how] 
		if(len a != 6 && len a != 7)
			error(sprint("reshape needs 6 or 7 params, saw %d", len a));
		err := reshape(w);
		if(err != nil)
			error(err);
	"!move" =>
		# !move tag reqid startx starty
		# dragging window started, ignore reqid, startx & starty are start locations
		narg(cmd, 4, len a);
		pt := Point(int a[2], int a[3]);
		moving = ref (prevptr, w, wc, buf, pt);
		w.resizing = 1;
		return;
	"!size" =>
		# !size tag reqid startx starty
		# resize window, we sneakily send "!size ..." to wmctxt.ctl, hoping they forward it to tkclient's wmctl, which sends us this !size, so we can give it a new image with the size we want.
		narg(cmd, 4, len a);
		err := reshape(w);
		if(err != nil)
			error(err);
	"start" =>
		narg(cmd, 1, len a);
		i := findstr(start, a[0]);
		if(i < 0)
			error(sprint("cannot start %q", a[0]));
		bit := 1<<i;
		if(w.started & bit)
			error(sprint("%q already started", a[0]));
		if(bit == Skbd && col.win == w)
			winkbd(w, 1);
		w.started |= bit;
	"key" =>
		# incoming simulated key
		narg(cmd, 1, len a);
		(v, rem) := str->toint(a[0], 10);
		if(rem != nil)
			error(sprint("key not decimal: %#q", a[0]));
		spawn keysend(v);
	"delete" =>
		narg(cmd, 1, len a);
		tag := a[0];
		say(sprint("xxx delete window %#q", tag));
		# what should we do about this?
	"fixedorigin" =>
		# don't change origin when moving, give new image entirely
		narg(cmd, 0, len a);
		w.fixedorigin = 1;
	"kbdfocus" =>
		if(len a != 0 && len a != 1)
			error(sprint("kbdfocus needs 0 or 1 arguments, saw %d", len a));
		focus := 0;
		if(len a == 1) {
			(v, rem) := str->toint(a[0], 10);
			if(rem != nil)
				error(sprint("bad kbdfocus arg %#q", a[0]));
			focus = v;
		}
		if(w == col.win) {
			nw := col.wins[(w.index+1)%len col.wins];
			winkbd(nw, 1);
			col.win = nw;
			ptrensure(col, col.win);
		}
	"lower" or
	"task" =>
		if(w.wantr.dy() > Winmin)
			winmin(cols[w.colindex], w);
	"raise" or
	"unhide" =>
		winunhide(cols[w.colindex], w);
		otherwin = w.tag;
	* =>
		error(sprint("unrecognized request %#q", cmd));
	}

	wc <-= (len buf, nil);
	if(doimg) {
		say(sprint("sending image, r %s, wantr %s", r2s(w.img.r), r2s(w.wantr)));
		w.nbwm.images <-= w.img;
	}
}

reshape(w: ref Win): string
{
	w.resizing = 0;
	new := w.img == nil || (!w.wantr.eq(w.haver) || w.fixedorigin && !w.wantr.eq(w.img.r));
	if(new) {
		if(w.img == nil)
			say(sprint("newimg, wantr %s, img nil", r2s(w.wantr)));
		else
			say(sprint("newimg, wantr %s, img.r %s", r2s(w.wantr), r2s(w.img.r)));
		nimg := drawctxt.screen.newwindow(w.wantr, draw->Refnone, draw->Grey);
		if(nimg == nil)
			return sprint("newwindow %s: %r", r2s(w.wantr));
		nimg.bottom();
		w.img = nimg;
	} else {
		say("setting new origin");
		w.img.origin(w.wantr.min, w.wantr.min);
		w.haver = w.wantr;
	}
	w.haver = w.wantr;
	return nil;
}


winmk(fid: int): ref Win
{
	kbd := chan[128] of int;
	ptr := chan of ref Pointer;
	ctl := chan of string;
	wctl := chan of string;
	images := chan of ref Image;
	wm := ref Wmcontext(kbd, ptr, ctl, wctl, images, nil, drawctxt);

	nbwm := ref *wm;
	nbwm.kbd = kbd;
	nbwm.ptr = chan of ref Pointer;
	nbwm.ctl = chan of string;
	nbwm.images = chan of ref Image;

	w := ref Win(-1, -1, sprint("w%d", wingen++), fid, wm, nbwm, nil, 0, nil, zerorect, zerorect, zerorect, 0, 0);

	pidc := chan of int;
	spawn chanbuf(nbwm.ptr, wm.ptr, pidc);
	spawn chanbuf(nbwm.ctl, wm.ctl, pidc);
	spawn chanbuf(nbwm.images, wm.images, pidc);
	spawn wctlfwd(w, pidc);
	w.pids = list of {<-pidc, <-pidc, <-pidc, <-pidc};

	otherwin = w.tag;
	winadd(col, w, col.r.dy()/2);
	resize();
	if(col.mode == Msingle || len col.wins == 1)
		focus(col, w, 0);
	else
		ptrensure(col, col.win);
	return w;
}

# add w, give it at least height h.
# we add it before col.win if present.
# caller is responsible for col.win
winadd(c: ref Col, w: ref Win, h: int)
{
	w.colindex = c.index;
	i := 0;
	if(c.win != nil)
		i = c.win.index;
	c.wins = concat(concat(c.wins[:i], array[] of {w}), c.wins[i:]);
	renumber(c.wins);
	layout(c, w, h);
}

renumber(w: array of ref Win)
{
	for(i := 0; i < len w; i++)
		w[i].index = i;
}

# delete win from col.  set new col.win.
# we give the height to win below, or otherwise above removed win,
# to keep the number of changed resizes limited.
# caller is responsible for telling focus changes to win, and resize.
windel(c: ref Col, w: ref Win)
{
	i := w.index;
	if(find(c.wins, w) != i)
		raise "inconstency";
	c.wins = concat(c.wins[:i], c.wins[i+1:]);
	renumber(c.wins);
	# this takes care of:  0,1,any wins left, for modes Mstack,Msingle
	say(sprint("windel, i %d, w.index %d, len c.wins %d", i, w.index, len c.wins));
	if(i < len c.wins) {
		# give to top of win below the removed win
		nw := c.wins[i];
		nw.layoutr.min.y -= w.layoutr.dy();
		nw.wantr.min.y -= w.wantr.dy();
		c.win = nw;
	} else if(i-1 >= 0) {
		# give to bottom of win above removed win
		say(sprint("windel, i %d, i-1 %d, giving %d", i, i-1, w.layoutr.dy()));
		nw := c.wins[i-1];
		nw.layoutr.max.y += w.layoutr.dy();
		nw.wantr.max.y += w.wantr.dy();
		c.win = nw;
	} else {
		# no wins left
		c.win = nil;
	}
}

getviswidths(): array of int
{
	a := array[len vis] of int;
	for(i := 0; i < len a; i++)
		a[i] = vis[i].r.dx();
	return a;
}

setviswidths(a: array of int)
{
	ocols := array[len cols] of ref Col;
	ocols[:] = cols;

	x := 0;
	for(i := 0; i < len a; i++) {
		c := vis[i];
		ocols[c.index] = nil;  # skip later
		c.tagr.min.x = c.r.min.x = c.fr.min.x = x;
		x += a[i];
		c.tagr.max.x = c.r.max.x = c.fr.max.x = x;
		for(j := 0; j < len c.wins; j++) {
			w := c.wins[j];
			w.layoutr.min.x = w.wantr.min.x = c.r.min.x;
			w.layoutr.max.x = w.wantr.max.x = c.r.max.x;
		}
	}
	for(i = 0; i < len ocols; i++) {
		c := ocols[i];
		if(c == nil)
			continue;
		c.tagr.min.x = c.r.min.x = c.fr.min.x = 0;
		c.tagr.max.x = c.r.max.x = c.fr.max.x = 0;
		for(j := 0; j < len c.wins; j++) {
			w := c.wins[j];
			w.layoutr.min.x = w.wantr.min.x = 0;
			w.layoutr.max.x = w.wantr.max.x = 0;
		}
	}
}

getheights(c: ref Col): array of int
{
	a := array[len c.wins] of int;
	for(i := 0; i < len a; i++)
		a[i] = c.wins[i].layoutr.dy();
	return a;
}

setheights(c: ref Col, a: array of int)
{
	y := c.r.min.y;
	for(i := 0; i < len c.wins; i++) {
		w := c.wins[i];
		w.wantr = Rect((c.r.min.x, y), (c.r.max.x, y+a[i]));
		if(c.mode == Mstack)
			w.layoutr = w.wantr;
		y += a[i];
	}
	resize();
}

renumbervis()
{
	for(i := 0; i < len cols; i++)
		cols[i].visindex = -1;
	for(i = 0; i < len vis; i++)
		vis[i].visindex = i;
}

visset(n: array of ref Col, w: array of int)
{
	vis = n;
	renumbervis();
	setviswidths(w);
	resize();
}

resize()
{
	for(j := 0; j < len cols; j++) {
		c := cols[j];
		for(i := 0; i < len c.wins; i++) {
			w := c.wins[i];
			if(!w.resizing && w.img != nil && !w.wantr.eq(w.haver)) {
				winctl(w, "!size . -1 0 0");
				w.resizing = 1;
			}
		}
	}
}


# xi was dragged onto yi, released at off.
movepast[T](oa: array of T, osz: array of int, xi, yi, off: int): (array of T, array of int)
{
	na: array of T;
	nsz: array of int;

	xt := array[] of {oa[xi]};
	if(xi < yi) {
		ndy := sum(osz[:yi+1])-off;
		szd := ndy-osz[xi];
		osz[xi] += szd;
		osz[yi] -= szd;
		xsz := array[] of {osz[xi]};
		na = concat(oa[:xi], concat(oa[xi+1:yi+1], concat(xt, oa[yi+1:])));
		nsz = concati(osz[:xi], concati(osz[xi+1:yi+1], concati(xsz, osz[yi+1:])));
	} else {
		szd := sum(osz[:yi+1])-off;
		osz[yi] -= szd;
		osz[xi] += szd;
		xsz := array[] of {osz[xi]};
		na = concat(oa[:yi+1], concat(xt, concat(oa[yi+1:xi], oa[xi+1:])));
		nsz = concati(osz[:yi+1], concati(xsz, concati(osz[yi+1:xi], osz[xi+1:])));
	}
	return (na, nsz);
}

colsetleft(c: ref Col, x: int)
{
	i := c.visindex;
	if(i == 0)
		return;
	w := getviswidths();
	n := c.r.min.x-x;
	w[i] += n;
	w[i-1] -= n;
	setviswidths(w);
	resize();
}


colbox(c: ref Col): Rect
{
	min := c.tagr.min.add((3,3));
	d := c.tagr.dy()-2*3;
	max := min.add((d, d));
	return Rect((min, max));
}

drawtags()
{
	for(i := 0; i < len cols; i++)
		drawtag(cols[i]);
}

drawtag(c: ref Col)
{
	bg := drawctxt.display.color(Tagcolor);
	selbg := drawctxt.display.color(Tagselcolor);
	boxc := drawctxt.display.color(Tagselcolor);
	white := drawctxt.display.white;
	opaque := drawctxt.display.opaque;

	if(c.tag == nil || c.tagr.min.x != c.tag.r.min.x || c.tagr.max.x != c.tag.r.max.x) {
		ntag := drawctxt.screen.newwindow(c.tagr, draw->Refnone, Tagcolor);
		if(ntag == nil)
			return warn(sprint("newwindow for tag: %r"));
		else
			c.tag = ntag;
	} else
		c.tag.draw(c.tag.r, bg, opaque, zeropt);

	box := colbox(c);
	c.tag.draw(box, boxc, opaque, zeropt);

	b := bg;
	if(col == c)
		b = selbg;
	p := Point(box.max.x+3, c.tag.r.min.y+1);
	p = c.tag.text(p, white, zeropt, tagfont, " ");
	p = c.tag.textbg(p, white, zeropt, tagfont, sprint("%d ", c.index+1), b, zeropt);
	c.moder = Rect((p.x, c.tagr.min.y), (0, c.tagr.max.y));
	m := modes[c.mode];
	p = c.tag.textbg(p, white, zeropt, tagfont, m, b, zeropt);
	s := "       ";
	p = c.tag.text(p, white, zeropt, tagfont, s[len m:]);
	c.moder.max.x = p.x;
	c.tagwins = array[len c.wins] of Rect;
	slackwidth := tagfont.width(" ")/2;
	for(i := 0; i < len c.wins; i++) {
		b = bg;
		if(c.win == c.wins[i])
			b = selbg;
		p = c.tag.textbg(p, white, zeropt, tagfont, " ", bg, zeropt);
		c.tagwins[i] = c.tagr;
		c.tagwins[i].min.x = p.x-slackwidth;
		p = c.tag.textbg(p, white, zeropt, tagfont, sprint("%d", i), b, zeropt);
		c.tagwins[i].max.x = p.x+slackwidth;
	}

	c.tagcmds = array[len tagcmds] of Rect;
	i = 0;
	pad := "  ";
	for(l := tagcmds; l != nil; l = tl l) {
		(tag, nil) := *hd l;
		p = c.tag.textbg(p, white, zeropt, tagfont, pad, bg, zeropt);
		pad = " ";
		c.tagcmds[i] = c.tagr;
		c.tagcmds[i].min.x = p.x-slackwidth;
		p = c.tag.textbg(p, white, zeropt, tagfont, tag, bg, zeropt);
		c.tagcmds[i].max.x = p.x+slackwidth;
		i += 1;
	}
}

# place focus on col/win where ptr is
focusptr()
{
	(c, w) := winfindpt(prevptr.xy);
	if(w == nil && len c.wins != 0)
		w = c.wins[0];
	focus(c, w, 0);
}

# if ptr is not on w, move it there.  if win is nil, make sure ptr is on c.
ptrensure(c: ref Col, w: ref Win)
{
	if(w == nil) {
		say(sprint("ptrensure, c %d, colbox %s, mid %s", c.index, r2s(colbox(c)), p2s(rectmid(colbox(c)))));
		return ptrset(rectmid(colbox(c)));
	}
	(x, y) := prevptr.xy;
	if(w.wantr.contains((x,y)))
		return;
	(nx, ny) := w.wantr.min.add((w.wantr.dx()/2, 10));
	opts := array[] of {(x,ny), (nx,y), (nx,ny)};
	for(i := 0; i < len opts; i++)
		if(w.wantr.contains(opts[i]))
			return ptrset(opts[i]);
	ptrset(rectmid(c.tagr));
}

# move focus to w in c.  or just c if w is nil.
# sets col.win.
# also sends haskbdfocus messages.
# if c is not focused, shows it too.
focus(c: ref Col, w: ref Win, ensptr: int)
{
	if(w == nil && len c.wins != 0)
		raise "bad focus";
	if(col.win != w) {
		if(col.win != nil) {
			winkbd(col.win, 0);
			otherwin = col.win.tag;
		}
		if(w != nil) {
			if(c.mode == Msingle) {
				h := array[len c.wins] of {* => 0};
				h[w.index] = c.r.dy();
				setheights(c, h);
				resize();
			} else if(c.mode == Mstack && w.wantr.dy() < Winmin) {
				h := getheights(c);
				want := Winmin-w.wantr.dy();
				taken := 0;
				taken += scavenge(want-taken, Winmin, 0, w.index, 1, h);
				taken += scavenge(want-taken, Winmin, w.index+1, len c.wins, 1, h);
				taken += scavenge(want-taken, 0, 0, w.index, 1, h);
				taken += scavenge(want-taken, 0, w.index+1, len c.wins, 1, h);
				h[w.index] += taken;
				setheights(c, h);
				resize();
			}
			winkbd(w, 1);
		}
	}
	col = c;
	col.win = w;
	if(ensptr)
		ptrensure(col, w);
}


# change layout to add w (already in c.wins).  give w at least height 'want'.
# c.win is not correct, don't use.
# w is only at the end of col if it is the only window.
layout(c: ref Col, w: ref Win, want: int)
{
	if(len c.wins == 1) {
		w.wantr = w.layoutr = c.r;
		return;
	}

	# pretend new win is empty, above col.win
	w.layoutr = c.wins[w.index+1].layoutr;
	w.layoutr.max.y = w.layoutr.min.y;

	oh := getheights(c);
	nh := array[len c.wins] of {* => 0};
	nh[w.index] = want;
	left := c.r.dy()-want;

	# give out max Winmin to all wins except w
	maxper := min(Winmin, left/(len nh-1));
	for(i := 0; i < len nh; i++)
		if(i != w.index) {
			nh[i] = min(oh[i], maxper);
			left -= nh[i];
		}

	# hand out last height from current win upwards (skipping w),
	# to past current win downwards.
	for(i = w.index+1; left > 0 && i >= 0; i--) {
		if(i == w.index)
			continue;
		give := min(left, oh[i]-nh[i]);
		nh[i] += give;
		left -= give;
	}
	for(i = w.index+1; left > 0 && i < len c.wins; i++) {
		give := min(left, oh[i]-nh[i]);
		nh[i] += give;
		left -= give;
	}
	setheights(c, nh);
}

# scavange up to 'want' in total, keeping 'keep' per entry, starting at index 's',
# ending at index 'e' (exclusive), width 'delta' as direction (1 or -1).
scavenge(want, keep, s, e, delta: int, sz: array of int): int
{
	if(delta > 0 && s > e || delta < 0 && s < e)
		return 0;
	taken := 0;
	while(want > taken && s >= 0 && s < len sz) {
		take := min(want-taken, max(0, sz[s]-keep));
		sz[s] -= take;
		taken += take;
		s += delta;
		if(s == e)
			break;
	}
	return taken;
}

# condense windows s-e (exclusive), expand n by that much.
# s & e are window indices, e may come before s.
condense(c: ref Col, s, e, n, min: int)
{
	height := c.r.dy();
	want := height/8;

	delta := 1;
	if(s > e)
		delta = -1;
	h := getheights(c);
	taken := scavenge(want, Winmin, s, e, delta, h);
	if(taken == 0)
		taken = scavenge(want, min, s, e, delta, h);
	if(taken == 0)
		return;
	h[n] += taken;
	setheights(c, h);
}

# move w from oc to nc
winmovecol(w: ref Win, oc: ref Col, nc: ref Col, y: int)
{
	windel(oc, w);
	col = nc;
	if(y < nc.r.min.y)
		y = nc.r.min.y;

	h: array of int;
	(nil, ow) := winfindpt(Point(nc.r.min.x, y));
	if(ow == nil && len nc.wins != 0)
		ow = nc.win;
	if(ow != nil) {
		h = getheights(nc);
		nc.wins = concat(concat(nc.wins[:ow.index+1], array[] of {w}), nc.wins[ow.index+1:]);
		dh := ow.wantr.max.y-y;
		h[ow.index] -= dh;
		h = concati(concati(h[:ow.index+1], array[] of {dh}), h[ow.index+1:]);
	} else {
		nc.wins = array[] of {w};
	}
	w.colindex = nc.index;
	renumber(nc.wins);
	if(nc.mode != Mstack || len nc.wins == 1) {
		h = array[len nc.wins] of {* => 0};
		h[w.index] = nc.r.dy();
	}
	nc.win = w;
	setheights(nc, h);
	resize();
}

winbigger(c: ref Col, w: ref Win, pt: Point, canmove: int, want: int)
{
	oy := w.wantr.min.y;
	say(sprint("bigger win %d", w.index));
	h := getheights(c);
	taken := scavenge(want, Winmin, w.index+1, len c.wins, 1, h);
	taken += scavenge(want-taken, Winmin, 0, w.index, 1, h);
	h[w.index] += taken;
	setheights(c, h);
	resize();
	focus(c, w, 0);

	if(canmove && oy != w.wantr.min.y)
		ptrset(pt.add((0, w.wantr.min.y-oy)));
}

winmax(c: ref Col, w: ref Win)
{
	say(sprint("biggest win %d", w.index));
	h := getheights(c);
	left := c.r.dy();
	nh := array[len h] of int;
	nh[:] = h;
	nh[w.index] = left/2;
	left -= nh[w.index];
	for(i := 0; i < len h; i++)
		if(i != w.index) {
			nh[i] = min(left, Winmin);
			left -= nh[i];
		}
	nh[w.index] += left;
	setheights(c, nh);
	resize();
	focus(c, w, 0);
}

winmin(c: ref Col, w: ref Win)
{
	say(sprint("min win %d", w.index));
	if(len c.wins == 1 || w.wantr.dy() <= Winmin)
		return;
	ni := (w.index-1+len c.wins)%len c.wins;
	if(c.mode == Msingle) {
		nw := c.wins[ni];
		w.wantr = zerorect;
		nw.wantr = c.r;
		focus(c, nw, 0);
		resize();
	} else {
		h := getheights(c);
		take := max(0, w.wantr.dy()-Winmin);
		# find first win (first up, than down) that isn't already minimized
		for(i := w.index-1; i >= 0 && h[i] <= Winmin; i--)
			{}
		if(i < 0)
			for(i = w.index+1; i < len c.wins && h[i] <= Winmin; i++)
				{}
		if(i < 0 || i >= len c.wins)
			i = ni;
		h[w.index] -= take;
		h[i] += take;
		nw := c.wins[i];
		setheights(c, h);
		resize();
		focus(c, nw, 0);
		ptrset((prevptr.xy.x, nw.wantr.min.y+10));
	}
}

winunhide(c: ref Col, w: ref Win)
{
	say(sprint("unhide, c.index %d, c.visindex %d", c.index, c.visindex));
	if(c.visindex < 0) {
		say("toggling col");
		coltoggle(c);
	}
	if(w.wantr.dy() <= 2*Winmin) {
		want := c.r.dy()/4;
		h := getheights(c);
		taken := scavenge(want, Winmin, w.index+1, len c.wins, 1, h);
		taken += scavenge(want-taken, Winmin, 0, w.index, 1, h);
		h[w.index] += taken;
		setheights(c, h);
		resize();
	}
	ptrensure(col, col.win);
}

winsingle(c: ref Col, w: ref Win)
{
	say(sprint("single win %d", w.index));
	h := array[len c.wins] of {* => 0};
	h[w.index] = c.r.dy();
	setheights(c, h);
	resize();
	focus(c, w, 0);
}

# make col bigger.
# if it is rightmost, start taking space from the left.
# otherwise, start taking from the right.
colbigger(c: ref Col)
{
	ox := c.tagr.min.x;
	want := tagsrect.dx()/8;
	keep := Colmin;
	w := getviswidths();
	i := c.visindex;
	if(i == len vis-1)
		taken := scavenge(want, keep, 0, i, 1, w);
	else {
		taken = scavenge(want, keep, len vis-1, i, -1, w);
		taken += scavenge(want-taken, keep, 0, i, 1, w);
	}
	w[i] += taken;
	setviswidths(w);
	resize();
	if(ox != c.tagr.min.x)
		ptrbox(c);
}

# make col as big as possible, leaving max Colmin for the others
colmax(c: ref Col)
{
	ox := c.tagr.min.x;

	w := getviswidths();
	j := c.visindex;
	given := 0;
	for(i := 0; i < len w; i++)
		if(i != j) {
			w[i] = min(w[i], Colmin);
			given += w[i];
		}
	w[j] = tagsrect.dx()-given;
	setviswidths(w);
	resize();
	if(ox != c.tagr.min.x)
		ptrbox(c);
}

cfgget(): ref Cfg
{
	a := array[len vis] of ref Col;
	a[:] = vis;
	tag := "";
	if(col.win != nil)
		tag = col.win.tag;
	return ref Cfg(a, getviswidths(), col, tag);
}

cfgset(cfg: ref Cfg)
{
	if(othercfg == nil || !configeq(othercfg, cfg))
		othercfg = cfg;
}

configeq(a, b: ref Cfg): int
{
	if(len a.c != len b.c)
		return 0;
	for(i := 0; i < len a.c; i++)
		if(a.c[i] != b.c[i])
			return 0;
	return 1;
}

# switch to previous col configuration
colswitch()
{
	if(othercfg == nil || configeq(cfgget(), othercfg))
		return;

	(c, w) := winfindtag(othercfg.tag);
	if(w != nil && find(othercfg.c, c) < 0)
		w = nil;
	visset(othercfg.c, othercfg.w);
	focus(othercfg.col, w, 1);
}

# toggle visibility of c
coltoggle(c: ref Col)
{
	say(sprint("toggle col %d", c.index));
	i := c.visindex;
	if(i < 0) {
		# add c
		ci := col.visindex;
		w := getviswidths();
		avail := tagsrect.dx()/(len vis+1);
		keep := min(avail, Colmin);
		take := scavenge(avail, keep, ci+1, len vis, 1, w);
		take += scavenge(avail-take, keep, 0, ci+1, 1, w);
		nv := concat(vis[:ci+1], concat(array[] of {c}, vis[ci+1:]));
		nw := concati(w[:ci+1], concati(array[] of {take}, w[ci+1:]));
		visset(nv, nw);
		ptrensure(col, col.win);
		return;
	}
	if(len vis == 1)
		return;  # cannot remove last col

	# remove col
	w := getviswidths();
	ni := i+1;
	if(i+1 >= len w)
		ni = i-1;
	w[ni] += w[i];
	nv := concat(vis[:i], vis[i+1:]);
	nw := concati(w[:i], w[i+1:]);
	visset(nv, nw);
	focusptr();
}

# show non-empty columns only
colnonempty()
{
	n, a: list of ref Col;
	for(i := 0; i < len cols; i++) {
		c := cols[i];
		if(len c.wins == 0)
			continue;
		if(c.visindex < 0)
			a = c::a;
		else
			n = c::n;
	}
	if(len n == len vis && a == nil)
		return;
	if(n == nil && a == nil)
		a = cols[0]::nil;
	ow := getviswidths();
	nw := array[len n+len a] of int;
	nv := l2a(n);
	left := tagsrect.dx();
	for(i = 0; i < len nv; i++) {
		nw[i] = ow[nv[i].visindex];
		left -= nw[i];
	}
	need := len a*Colmin;
	keep := min(tagsrect.dx()/(len n+len a), Colmin);
	left += scavenge(need-left, keep, 0, len nv, 1, nw[:len nv]);
	nv = concat(nv, l2a(a));

	# give space to new cols
	for(; i < len nv; i++)
		left -= nw[i] = left/len a;

	# give remaining space to current or new col
	i = find(nv, col);
	if(i < 0)
		i = 0;
	nw[i] += left;
	visset(nv, nw);
	if(col.visindex < 0)
		focus(vis[0], vis[0].win, 0);
}

colsingle(c: ref Col)
{
	visset(array[] of {c}, array[] of {tagsrect.dx()});
	focus(c, c.win, 0);
}

modesingle()
{
	col.mode = Msingle;
	if(col.win == nil)
		return;
	sz := array[len col.wins] of {* => 0};
	sz[col.win.index] = col.r.dy();
	setheights(col, sz);
}

modestack()
{
	col.mode = Mstack;
	for(i := 0; i < len col.wins; i++)
		col.wins[i].wantr = col.wins[i].layoutr;
	resize();
	ptrensure(col, col.win);
}

modetoggle()
{
	case col.mode {
	Mstack =>	modesingle();
	Msingle =>	modestack();
	}
}


# return col & win ptr is over.  win may be nil.
winfindpt(p: Point): (ref Col, ref Win)
{
	for(j := 0; j < len cols; j++) {
		ww := cols[j].wins;
		for(i := 0; i < len ww; i++)
			if(ww[i].wantr.contains(p))
				return (cols[j], ww[i]);
		if(cols[j].fr.contains(p))
			return (cols[j], nil);
	}
	return (nil, nil);
}

winfindtag(tag: string): (ref Col, ref Win)
{
	for(j := 0; j < len cols; j++) {
		wins := cols[j].wins;
		for(i := 0; i < len wins; i++)
			if(wins[i].tag == tag)
				return (cols[j], wins[i]);
	}
	return (nil, nil);
}

winfindfid(fid: int): ref Win
{
	for(j := 0; j < len cols; j++) {
		wins := cols[j].wins;
		for(i := 0; i < len wins; i++)
			if(wins[i].fid == fid)
				return wins[i];
	}
	return nil;
}

rectmid(r: Rect): Point
{
	return r.min.add((r.dx()/2, r.dy()/2));
}

rectindex(a: array of Rect, pt: Point): int
{
	for(i := 0; i < len a; i++)
		if(a[i].contains(pt))
			return i;
	return -1;
}

r2s(r: Rect): string
{
	return sprint("%d %d %d %d", r.min.x, r.min.y, r.max.x, r.max.y);
}

p2s(p: Point): string
{
	return sprint("%d %d", p.x, p.y);
}

l2a[T](l: list of T): array of T
{
	a := array[len l] of T;
	i := 0;
	for(; l != nil; l = tl l)
		a[i++] = hd l;
	return a;
}

rev[T](l: list of T): list of T
{
	r: list of T;
	for(; l != nil; l = tl l)
		r = hd l::r;
	return r;
}

find[T](a: array of T, e: T): int
{
	for(i := 0; i < len a; i++)
		if(a[i] == e)
			return i;
	return -1;
}

findstr(a: array of string, s: string): int
{
	for(i := 0; i < len a; i++)
		if(a[i] == s)
			return i;
	return -1;
}

sum(a: array of int): int
{
	v := 0;
	for(i := 0; i < len a; i++)
		v += a[i];
	return v;
}

concat[T](a, b: array of T): array of T
{
	n := array[len a+len b] of T;
	n[:] = a;
	n[len a:] = b;
	return n;
}

concati(a, b: array of int): array of int
{
	n := array[len a+len b] of int;
	n[:] = a;
	n[len a:] = b;
	return n;
}

killall(l: list of int)
{
	for(; l != nil; l = tl l)
		kill(hd l);
}

kill(pid: int)
{
	sys->fprint(sys->open(sprint("/prog/%d/ctl", pid), Sys->OWRITE), "kill");
}

abs(i: int): int
{
	if(i < 0)
		i = -i;
	return i;
}

min(a, b: int): int
{
	if(a < b) return a;
	return b;
}

max(a, b: int): int
{
	if(a > b) return a;
	return b;
}

pid(): int
{
	return sys->pctl(0, nil);
}

warn(s: string)
{
	if(fd2 == nil)
		fd2 = sys->fildes(2);
	sys->fprint(fd2, "%s\n", s);
}

say(s: string)
{
	if(dflag)
		warn(s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
