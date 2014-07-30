'use strict';

module.factory('slotClockService', [
  '$timeout', function ($timeout) {
    // Clock

    var Clock = function () {
      var clock = this;

      this.playFns = [];
      this.pauseFns = [];
      this.timeFns = [];

      this.begun = 0;

      Object.defineProperties(this, {
        position: {
          get: function () {
            return Date.now() - clock.begun;
          }
        }
      });

      // ticker
      this.interval = 25;
      this.ticker = null;
    };

    // Clock behaviors

    Clock.prototype.play = function () {
      this.begun = Date.now();
      tickerOn(this);
      callFn(this, this.playFns);
    };

    Clock.prototype.playFn = function (fn) {
      return registerFn(this, this.playFns, fn);
    };

    //

    Clock.prototype.pause = function () {
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
        clock.ticker = $timeout(ticker(clock), clock.interval);
      };
    };

    var tickerOn = function (clock) {
      if (clock.ticker) {
        tickerOff(clock);
      }

      clock.ticker = $timeout(ticker(clock), clock.interval);
    };

    var tickerOff = function (clock) {
      return $timeout.cancel(clock.ticker);
    };

    // Service

    return Clock;
  }
]);
