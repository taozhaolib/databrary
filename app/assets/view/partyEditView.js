'use strict';

module.controller('partyEditView', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    $scope.party = party;
    page.display.title = page.constants.message('page.title.party.edit');

    page.display.toolbarLinks.push({
      type: 'yellow',
      html: page.constants.message('party.view'),
      url: party.route,
    });

    $scope.forms = {};
    $scope.registerStep = function (step) {
      step.form = $scope.forms['partyEdit' + step.name.charAt(0).toUpperCase() + step.name.slice(1) + 'Form'];
    };

    $scope.switchStep = function (step) {
      var cur = $scope.activeStep;

      if (cur && cur.form.$dirty && !cur.form.resetAll())
	return false;

      //to avoid bug where "float" elements fixed to top of page at lower scrolls are already at top
      if (step.form.scrollFn)
	page.$timeout(step.form.scrollFn);

      return true;
    };

    page.display.navigationFn = function () {
      return !$scope.activeStep.form.$dirty;
    };

    $scope.$watch(function () {
      $scope.steps.forEach(function (step) {
	step.complete = !step.form.$dirty;
	step.error = step.form.$invalid;
      });
    });
  }
]);
