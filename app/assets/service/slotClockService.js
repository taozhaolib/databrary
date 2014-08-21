'use strict';

module.factory('slotClockService', [
  '$timeout', function ($timeout) {
    // Clock

    var Clock = function (slot, ctrl) {
      var clock = this;
      this.slot = slot;
      this.ctrl = ctrl;

      this.playFns = [];
      this.pauseFns = [];
      this.timeFns = [];
      this.jumpFns = [];

      this.begun = Date.now();
      this.changed = this.begun;
      this.duration = 0;

      Object.defineProperties(this, {
        position: {
          get: function () {
            clock.changed = Date.now() - clock.begun;
            return clock.changed < clock.duration ? clock.changed : clock.duration;
          }
        }
      });

      slot.assets.forEach(function (asset) {
        // TODO: hack until type classes
        asset.container = slot.container;

        if (ctrl.hasDuration(asset)) {
          clock.duration = clock.duration >= asset.segment[1] ? clock.duration : asset.segment[1];
        } else if (ctrl.hasPosition(asset)) {
	  clock.duration = clock.duration >= asset.segment ? clock.duration : asset.segment;
	}
      });

      // ticker
      this.interval = 100;
      this.playing = false;
    };

    // Clock behaviors

    Clock.prototype.play = function (pos) {
      if (angular.isNumber(pos)) {
        this.jump(pos);
      }

      tickerOn(this);
      callFn(this, this.playFns);
    };

    Clock.prototype.playFn = function (fn) {
      return registerFn(this, this.playFns, fn);
    };

    //

    Clock.prototype.pause = function () {
      this.changed = this.position;
      tickerOff(this);
      callFn(this, this.pauseFns);
    };

    Clock.prototype.pauseFn = function (fn) {
      return registerFn(this, this.pauseFns, fn);
    };

    //

    Clock.prototype.time = function (pos) {
      if (angular.isNumber(pos)) {
        this.jump(pos);
      }

      callFn(this, this.timeFns);
    };

    Clock.prototype.timeFn = function (fn) {
      return registerFn(this, this.timeFns, fn);
    };

    //

    Clock.prototype.jump = function (pos) {
      if (angular.isNumber(pos)) {
        this.changed = Date.now();

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
