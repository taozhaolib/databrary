'use strict';

module.directive('video', [
  'pageService',
  function (page) { return {
    restrict: 'E',
    require: ['?^slotView'],
    link: function ($scope, $element, $attr, listeners) {
      $element.on('loadedmetadata', function () {
	var seek = $scope.$eval($attr.seek), stop;
	if (seek instanceof page.models.Segment) {
	  stop = seek.u;
	  seek = seek.l;
	}

	if (isFinite(seek) && seek > 0) {
	  this.currentTime = seek / 1000;
	  $element.off('loadedmetadata');
	}

	if (isFinite(stop) && stop > seek) {
	  stop /= 1000;
	  var checkStop = function() {
	    if (this.currentTime < stop)
	      return;
	    this.pause();
	    $element.off('timeupdate', checkStop);
	  };
	  $element.on('timeupdate', checkStop);
	}

	listeners.forEach(function (l) {
	  if (!l)
	    return;
	  l.registerVideo($element);
	  $scope.$on('$destroy', function () {
	    l.deregisterVideo($element);
	  });
	});
      });
    }
  }; }
]);
