'use strict';

module.factory('clockService', [
	'$timeout', function ($timeout) {
		// Clock

		var Clock = function ($scope, slot) {
			var clock = this;

			this.playFns = [];
			this.pauseFns = [];
			this.timeFns = [];

			this.position = 0;

			// ticker
			this.interval = 25;
			this.ticker = null;
		};

		// Clock behaviors

		Clock.prototype.play = function () {
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

			console.log(clock);

			clock.ticker = $timeout(ticker(clock), clock.interval);
		};

		var tickerOff = function (clock) {
			return $timeout.cancel(clock.ticker);
		};

		// Service

		return function ClockMaker($scope, slot) {
			return new Clock($scope, slot);
		};
	}
]);
