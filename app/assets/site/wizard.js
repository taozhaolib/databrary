/* globals Event */
'use strict';

app.directive('wizard', [
  '$location', '$timeout', 'tooltipService', 'messageService',
  function ($location, $timeout, tooltips, messages) {
    return {
      restrict: 'E',
      templateUrl: 'site/wizard.html',
      transclude: true,
      controller: ['$scope', '$attrs', function ($scope, $attrs) {
        this.name = $scope.wizardName = $attrs.name;
        $scope.steps = [];
        $scope.step = {};
        $scope.activeStep = undefined;

        var target = $location.search().page;
        this.addStep = function (step) {
          $scope.steps.push(step);
          $scope.step[step.name] = step;

          $scope.registerStep(step);

          if (!$scope.activeStep || target === step.name)
            $scope.activateStep(step);
        };

        $scope.activateStep = function (newStep) {
          if (!newStep.allow || $scope.activeStep === newStep)
            return;

          if ($scope.switchStep && !$scope.switchStep(newStep))
            return;

          if ($scope.activeStep)
            $scope.activeStep.active = false;
          $location.replace().search('page', newStep.name);
          $scope.activeStep = newStep;
          newStep.active = true;
          tooltips.clear();
          messages.clear();
          $timeout(function() {
            window.dispatchEvent(new Event('scroll'));
          });
        };
      }],
    };
  }
]);
