#include "av.h"

void avFrame_initialize_stream(AVStream *o, AVFormatContext *c, AVStream *i, AVFrame *f, int w, int h)
{
	o->codec->time_base = i->codec->time_base;
	o->time_base = i->time_base;
	AVRational sar = av_guess_sample_aspect_ratio(c, i, f);
	AVRational dsr = av_make_q(f->width, f->height);
	if (sar.num)
		dsr = av_mul_q(dsr, sar);
	else
		sar.num = sar.den = 1;
	if (f->height > (unsigned)h || sar.num > sar.den) {
		o->codec->height = (unsigned)h < f->height ? h : f->height;
		o->codec->width = o->codec->height * dsr.num / dsr.den;
	}
	if (!o->codec->width || o->codec->width > (unsigned)w) {
		o->codec->width = (unsigned)w < f->width ? w : f->width;
		o->codec->height = o->codec->width * dsr.den / dsr.num;
	}
	o->codec->colorspace = av_frame_get_colorspace(f);
	o->codec->color_range = av_frame_get_color_range(f);
}
