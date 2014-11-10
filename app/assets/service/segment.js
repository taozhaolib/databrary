'use strict';

app.factory('Segment', [
  'constantService',
  function (constants) {
    function Segment(l, u) {
      if (arguments.length >= 2) {
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
        this.u = 0;
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

    Object.defineProperty(Segment.prototype, 'length', {
      get: function () {
        return this.u - this.l;
      }
    });

    Object.defineProperty(Segment.prototype, 'lBounded', {
      get: function () {
        return this.l < this.u && isFinite(this.l);
      }
    });

    Object.defineProperty(Segment.prototype, 'uBounded', {
      get: function () {
        return this.u > this.l && isFinite(this.u);
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
    Segment.full = constants.deepFreeze(new Segment(undefined));

    Segment.isEmpty = function (x) {
      return x === null ||
        (x instanceof Segment && x.empty) ||
        x === '';
    };
    Segment.empty = constants.deepFreeze(new Segment(null));

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

    Segment.prototype.contains = function (that) {
      if (typeof that === 'number')
        return that >= this.l && that < this.u;
      if (Segment.isFull(that))
        return this.full;
      if (Segment.isEmpty(that))
        return !this.empty;
      that = Segment.make(that);
      return this.l <= that.l && this.u >= that.u;
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
        return 'empty';
      return seg.toString();
    };

    Segment.data = function (seg) {
      if (angular.isObject(seg) && !(seg instanceof Segment) && !Array.isArray(seg))
        return seg;
      return Segment.format(seg);
    };

    return Segment;
  }
]);
