'use strict';

module.factory('slotClockService', [
  '$timeout', function ($timeout) {
    // Clock

    var Clock = function (slot, ctrl) {
      var clock = this;
      clock.ctrl = ctrl;

      clock.playFns = [];
      clock.pauseFns = [];
      clock.timeFns = [];
      clock.jumpFns = [];

      Object.defineProperties(this, {
        position: {
          get: function () {
            var actual = clock.playing ? Date.now() - clock.begun : clock.changed - clock.begun;
            return actual < clock.duration ? actual : clock.duration;
          }
        }
      });

      clock.duration = 0;
      clock.start = undefined;

      slot.assets.forEach(function (asset) {
        // TODO: hack until type classes
        asset.container = slot.container;

        if (ctrl.hasDuration(asset)) {
          clock.duration = clock.duration >= asset.segment[1] ? clock.duration : asset.segment[1];
          clock.start = clock.start <= asset.segment[0] && angular.isNumber(clock.start) ? clock.start : asset.segment[0];
        } else if (ctrl.hasPosition(asset)) {
	  clock.duration = clock.duration >= asset.segment ? clock.duration : asset.segment;
	  clock.start = clock.start <= asset.segment && angular.isNumber(clock.start) ? clock.start : asset.segment;
	}
      });

      clock.begun = Date.now() - clock.start;
      clock.changed = this.begun;
      // ticker
      clock.interval = 100;
      clock.playing = false;
    };

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
      if (!angular.isFunction(fn))
        throw new Error('Clock callbacks must be callable.');

      if (fns.indexOf(fn) === -1) {
        return fns.push(fn);
      }

      return function cancelFn() {
        var index;

        if ((index = fns.indexOf(fn)) > -1) {
          return fns.splice(index, 1);
        }
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
