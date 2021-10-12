#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "image.h"

float get_pixel(image im, int x, int y, int c)
{
  if (x > im.w)
    x = im.w-1;
  if (x < 0)
    x = 0;
  if (y > im.h)
    y = im.h-1;
  if (y < 0)
    y = 0;
  if (c > im.c)
    c = im.c;
  if (c < 0)
    c = 0;

  return im.data[x + y*im.w + c*im.w*im.h];
}

void set_pixel(image im, int x, int y, int c, float v)
{
  if(x < im.w || y < im.h || c < im.c)
    im.data[c*im.w*im.h + y*im.w + x] = v;
}

image copy_image(image im)
{
    image copy = make_image(im.w, im.h, im.c);
    // memcpy(&copy.data, &im.data, sizeof(*im.data));
    for(int x = 0; x < im.w; x++) {
      for(int y = 0; y < im.h; y++) {
	for(int c = 0; c < im.c; c++) {
	  set_pixel(copy, x, y, c, get_pixel(im, x, y, c));
	}
      }
    }
    return copy;
}

image rgb_to_grayscale(image im)
{
    assert(im.c == 3);
    image gray = make_image(im.w, im.h, 1);
    float v;

    for(int x = 0; x < im.w; x++) {
      for(int y = 0; y < im.h; y++) {
	v = 0.299*get_pixel(im, x, y, 0)+0.587*get_pixel(im, x, y, 1)+0.114*get_pixel(im, x, y, 2);
	set_pixel(gray, x, y, 0, v);
      }
    }

    return gray;
}

void shift_image(image im, int c, float v)
{
  for(int x = 0; x < im.w; x++) {
    for(int y = 0; y < im.h; y++) {
      set_pixel(im, x, y, c, get_pixel(im, x, y, c)+v);
    }
  }
}

void clamp_image(image im)
{
  float v;

  for(int x = 0; x < im.w; x++) {
    for(int y = 0; y < im.h; y++) {
      for(int c = 0; c < im.c; c++) {
	v = get_pixel(im, x, y, c);
	if (v > 1) {
	  v = 1;
	} else if (v < 0) {
	  v = 0;
	}
	set_pixel(im, x, y, c, v);
      }
    }
  }
}


// These might be handy
float three_way_max(float a, float b, float c)
{
    return (a > b) ? ( (a > c) ? a : c) : ( (b > c) ? b : c) ;
}

float three_way_min(float a, float b, float c)
{
    return (a < b) ? ( (a < c) ? a : c) : ( (b < c) ? b : c) ;
}

void rgb_to_hsv(image im)
{
  float h1;
  float r,g,b = 0;
  float h,s,v,m,c = 0;
  for(int x = 0; x < im.w; x++) {
    for(int y = 0; y < im.h; y++) {
      r = get_pixel(im, x, y, 0);
      g = get_pixel(im, x, y, 1);
      b = get_pixel(im, x, y, 2);
      v = three_way_max(r, g, b);
      m = three_way_min(r,g,b);
      c = v - m;

      if (r==0 && g==0 && b==0) {
	s = 0;
      } else {
	s = c/v;
      }

      if (c==0) {
	h1 = 0;
      } else if (v==r) {
	h1 = (g-b)/c;
      } else if (v==g) {
	h1 = (b-r)/c + 2;
      } else if (v==b) {
	h1 = (r-g)/c + 4;
      }

      if (h1 < 0) {
	h = h1/6 + 1;
      } else {
	h = h1/6;
      }
      set_pixel(im, x, y, 0, h);
      set_pixel(im, x, y, 1, s);
      set_pixel(im, x, y, 2, v);
    }
  }
}

void hsv_to_rgb(image im)
{
  float h1, r, g, b;
  float h,s,v = 0;
  float x1,c,m = 0;
  for(int x = 0; x < im.w; x++) {
    for(int y = 0; y < im.h; y++) {
      h = get_pixel(im, x, y, 0);
      s = get_pixel(im, x, y, 1);
      v = get_pixel(im, x, y, 2);

      c = v*s;
      m = v-c;
      h1 = h/(60/360.0);

      x1 = c*(1-fabs((fmod(h1, 2)-1)));

      if (0 <= h1 && h1 <= 1) {
      	r = c;
      	g = x1;
      	b = 0;
      } else if (1 < h1 && h1 <= 2) {
	r = x1;
	g = c;
	b = 0;
      } else if (2 < h1 && h1 <= 3) {
	r = 0;
	g = c;
	b = x1;
      } else if (3 < h1 && h1 <= 4) {
	r = 0;
	g = x1;
	b = c;
      } else if (4 < h1 && h1 <= 5) {
	r = x1;
	g = 0;
	b = c;
      } else if (5 < h1 && h1 <= 6) {
	r = c;
	g = 0;
	b = x1;
      }

      r = r+m;
      g = g+m;
      b = b+m;

      set_pixel(im, x, y, 0, r);
      set_pixel(im, x, y, 1, g);
      set_pixel(im, x, y, 2, b);
    }
  }
}
