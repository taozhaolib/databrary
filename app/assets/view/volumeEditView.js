'use strict';

module.controller('volumeEditView', [
  '$scope', 'volume', 'pageService',
  function ($scope, volume, page) {
    $scope.volume = volume;
    page.display.title = volume ? volume.title : page.constants.message('page.title.volume');

    page.display.toolbarLinks.push({
      type: 'yellow',
      html: page.constants.message('volume.edit.' + (volume ? 'exit' : 'cancel')),
      url: volume ? volume.route : page.router.prevUrl
    });

    $scope.registerStep = function (step) {
      step.form = step.$scope['volumeEdit' + step.name.charAt(0).toUpperCase() + step.name.slice(1) + 'Form'];
    };

    function leavingSoSoon() {
      return !$scope.activeStep || $scope.activeStep.form.resetAll(false, true);
    }

    $scope.switchStep = function (step) {
      if (!leavingSoSoon())
	return false;

      //to avoid bug where "float" elements fixed to top of page at lower scrolls are already at top
      if (step.form.scrollFn)
	page.$timeout(step.form.scrollFn);

      return true;
    };

    var done = page.$rootScope.$on('$locationChangeStart', function (event, url) {
      /* hacky: */
      if (url.contains(volume ? volume.editRoute() : page.router.volumeCreate()))
	return;
      if (!leavingSoSoon())
	return event.preventDefault();
      done();
    });

    $scope.$watch(function () {
      $scope.steps.forEach(function (step) {
	if (step.form) {
	  step.complete = step.form.$pristine;
	  step.error = step.form.$invalid;
	}
      });
    });
  }
]);
