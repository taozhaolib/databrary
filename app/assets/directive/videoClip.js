'use strict';

module.directive('videoClip', [
  'Segment',
  function (Segment) { return {
    restrict: 'A',
    link: function ($scope, $element, $attr) {
      var segment = new Segment($scope.$eval($attr.videoClip));

      if (isFinite(segment.l) && segment.l) {
	$element.on('loadedmetadata', function () {
	  this.currentTime = segment.l / 1000;
	  $element.off('loadedmetadata');
	});
      }

      if (isFinite(segment.u) && segment.u > segment.l) {
	$element.on('timeupdate', function () {
	  if (this.currentTime < segment.u / 1000)
	    return;
	  this.pause();
	  $element.off('timeupdate');
	});
      }
    }
  }; }
]);
