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
	this.l = typeof l[0] === 'number' ? l[0] : -Infinity;
	this.u = typeof l[1] === 'number' ? l[1] : Infinity;
      } else if (angular.isNumber(l)) {
	this.l = l;
	this.u = l+0.1; // this is floored out later
      } else if (l === null) {
	this.l = 0;
	this.u = -1;
      } else if (l instanceof Object && 'l' in l && 'u' in l) {
	this.l = l.l;
	this.u = l.u;
      } else
	throw new Error('invalid Segment construction');
    }

    Object.defineProperty(Segment.prototype, 'full', {
      get: function () {
	return this.l === -Infinity && this.u === Infinity;
      }
    });

    Object.defineProperty(Segment.prototype, 'empty', {
      get: function () {
	return this.l >= this.u;
      }
    });

    Object.defineProperty(Segment.prototype, 'base', {
      get: function () {
	return isFinite(this.l) ? this.l : 0;
      }
    });

    Segment.isFull = function (x) {
      return x === undefined ||
	(x instanceof Segment && x.full) ||
	x === ',' || x === '-'; // handle strings just as an optimization
    };
    Segment.full = new Segment(undefined);

    Segment.isEmpty = function (x) {
      return x === null ||
	(x instanceof Segment && x.empty) ||
	x === '';
    };
    Segment.empty = new Segment(null);

    function base(x) {
      if (x instanceof Segment)
	return x.base;
      else if (Array.isArray(x))
	x = x[0];
      return isFinite(x) ? x : 0;
    }

    Segment.make = function(x) {
      return x instanceof Segment ? x : new Segment(x);
    };

    Segment.prototype.format = function () {
      if (this.full)
	return '-';
      if (this.empty)
	return '';
      var l = Math.floor(this.l);
      var u = Math.floor(this.u);
      if (l === u)
	return l.toString();
      return (isFinite(l) ? l : '') +
	',' + (isFinite(u) ? u : '');
    };
    
    Segment.prototype.toString = Segment.prototype.format;

    Segment.prototype.equals = function (that) {
      if (Segment.isFull(that))
	return this.full;
      if (Segment.isEmpty(that))
	return this.empty;
      if (typeof that === 'string')
	return false;
      that = Segment.make(that);
      return this.l === that.l && this.u === that.u;
    };

    Segment.prototype.intersect = function (that) {
      if (this.empty || Segment.isFull(that))
	return this;
      if (this.full || Segment.isEmpty(that))
	return that;
      that = Segment.make(that);
      return new Segment(
	  Math.max(this.l, that.l),
	  Math.min(this.u, that.u));
    };

    /* If segments are disjoint, assume the excluded middle. */
    Segment.prototype.union = function (that) {
      if (this.empty || Segment.isFull(that))
	return that;
      if (this.full || Segment.isEmpty(that))
	return this;
      that = Segment.make(that);
      return new Segment(
	  Math.min(this.l, that.l),
	  Math.max(this.u, that.u));
    };

    Segment.prototype.overlaps = function (that) {
      return !this.intersect(that).empty;
    };

    Segment.prototype.relativeTo = function (b) {
      b = base(b);
      return new Segment(this.l - b, this.u - b);
    };

    Segment.format = function (seg) {
      if (Segment.isFull(seg))
	return '-';
      if (Array.isArray(seg))
	return seg[0] + ',' + seg[1];
      if (Segment.isEmpty(seg))
	return '';
      return seg.toString();
    };

    return Segment;
  }
]);
