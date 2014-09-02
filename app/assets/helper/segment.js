'use strict';

module.factory('Segment', [
  function () {
    function Segment(l, u) {
      if (arguments.length === 2) {
	this.l = l;
	this.u = u;
      } else if (l === undefined) {
	this.l = -Infinity;
	this.u = Infinity;
      } else if (Array.isArray(l)) {
	this.l = angular.isNumber(l[0]) ? l[0] : -Infinity;
	this.u = angular.isNumber(l[1]) ? l[1] : Infinity;
      } else if (angular.isNumber(l)) {
	this.l = l;
	this.u = l+0.1; // this is floored out later
      } else if (l === null) {
	this.l = 0;
	this.u = -1;
      } else
	throw new Error('invalid Segment construction');
    }

    Object.defineProperty(Segment.prototype, 'empty', {
      get: function () {
	return this.l >= this.u;
      }
    });

    Segment.prototype.toString = function () {
      if (this.empty)
	return '';
      var l = Math.floor(this.l);
      var u = Math.floor(this.u);
      if (l === u)
	return l;
      return (l > -Infinity ? l : '') +
	',' + (u < Infinity ? u : '');
    };

    Segment.prototype.intersect = function (that) {
      return new Segment(
	  Math.max(this.l, that.l),
	  Math.min(this.u, that.u));
    };

    /* If segments are disjoint, assume the excluded middle. */
    Segment.prototype.union = function (that) {
      return new Segment(
	  Math.min(this.l, that.l),
	  Math.max(this.u, that.u));
    };

    Segment.prototype.overlaps = function (that) {
      return !this.intersect(that).empty;
    };

    Segment.prototype.relativeTo = function (base) {
      if (base instanceof Segment)
	base = base.l;
      else if (Array.isArray(base))
	base = base[0];
      base = isFinite(base) ? base : 0;
      return new Segment(this.l - base, this.u - base);
    };

    Segment.format = function (seg) {
      if (seg === undefined)
	return '-';
      if (Array.isArray(seg))
	return seg[0] + ',' + seg[1];
      if (seg === null)
	return '';
      return seg.toString();
    };

    return Segment;
  }
]);
