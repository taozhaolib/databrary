'use strict';

module.factory('slotClockService', [
  '$timeout', function ($timeout) {
    // Clock

    function Clock(slot, ctrl) {
      this.ctrl = ctrl;

      this.playFns = [];
      this.pauseFns = [];
      this.timeFns = [];
      this.jumpFns = [];

      this.duration = -Infinity;
      this.start = Infinity;

      slot.assets.forEach(function (asset) {
	if (isFinite(asset.segment.u) && asset.segment.u > this.duration)
	  this.duration = asset.segment.u;
	if (isFinite(asset.segment.l) && asset.segment.l < this.start)
	  this.start = asset.segment.l;
      }, this);

      this.begun = Date.now() - this.start;
      this.changed = this.begun;
      // ticker
      this.interval = 100;
      this.playing = false;
    }

    Object.defineProperties(Clock, {
      position: {
	get: function () {
	  var actual = this.playing ? Date.now() - this.begun : this.changed - this.begun;
	  return actual < this.duration ? actual : this.duration;
	}
      }
    });

    Clock.prototype.markChanged = function () {
      this.changed = Date.now() - this.start;
    };

    // Clock behaviors

    Clock.prototype.play = function () {
      this.begun = Date.now() - (this.changed - this.begun);
      tickerOn(this);
      callFn(this, this.playFns);
    };

    Clock.prototype.playFn = function (fn) {
      return registerFn(this, this.playFns, fn);
    };

    //

    Clock.prototype.pause = function () {
      this.markChanged();
      tickerOff(this);
      callFn(this, this.pauseFns);
    };

    Clock.prototype.pauseFn = function (fn) {
      return registerFn(this, this.pauseFns, fn);
    };

    //

    Clock.prototype.time = function () {
      callFn(this, this.timeFns);
    };

    Clock.prototype.timeFn = function (fn) {
      return registerFn(this, this.timeFns, fn);
    };

    //

    Clock.prototype.jump = function (pos) {
      if (angular.isNumber(pos)) {
        this.markChanged();

        if (pos < 0) {
          this.begun = this.changed;
        } else if (pos > this.duration) {
          this.begun = this.changed - this.duration;
        } else {
          this.begun = this.changed - pos;
        }
      }

      callFn(this, this.timeFns);
      callFn(this, this.jumpFns);
    };

    Clock.prototype.jumpFn = function (fn) {
      return registerFn(this, this.jumpFns, fn);
    };

    // Callback helpers

    var registerFn = function (clock, fns, fn) {
      if (fns.indexOf(fn) === -1) {
        return fns.push(fn);
      }

      return function cancelFn() {
	fns.remove(fn);
      };
    };

    var callFn = function (clock, fns) {
      angular.forEach(fns, function (fn) {
        fn(clock);
      });
    };

    // Ticker helpers

    var ticker = function (clock) {
      return function () {
        clock.time();

        if (clock.position >= clock.duration) {
          return clock.pause();
        }

        clock.playing = $timeout(ticker(clock), clock.interval);
      };
    };

    var tickerOn = function (clock) {
      if (clock.playing) {
        tickerOff(clock);
      }

      clock.playing = $timeout(ticker(clock), clock.interval);
    };

    var tickerOff = function (clock) {
      $timeout.cancel(clock.playing);
      clock.playing = false;
    };

    // Service

    return Clock;
  }
]);
