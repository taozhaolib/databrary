'use strict';

module.directive('wizard', [
  function () {
    var compile = function ($element, $attrs, transclude) {
      return function ($scope, $element) {
        if (angular.isFunction($scope.retrieve())) {
          $scope.retrieve()($scope);
        }

        //

        $scope.steps = [];
        $scope.stepsList = {};
        $scope.newStep = undefined;

        $scope.onFn = $scope.onFn || {};
        $scope.offFn = $scope.offFn || {};
        $scope.addFn = $scope.addFn || undefined;
        $scope.activateFn = $scope.activateFn || undefined;

        $scope.addStep = function (step) {
          $scope.steps.push(step);
          $scope.stepsList[step.id] = step;
          $scope.newStep = step;

          if (angular.isFunction($scope.update())) {
            $scope.update()();
          }
        };

        $scope.activateStep = function (step) {
          if ($scope.isStepBlocked(step)) {
            return;
          }

          if (angular.isFunction($scope.activateFn)) {
            $scope.activateFn(step, $scope);
          }

          angular.forEach($scope.steps, function (thisStep) {
            if (thisStep.active && $scope.offFn[step.id] && angular.isFunction($scope.offFn[step.id])) {
              $scope.offFn[step.id](thisStep, step);
            }

            thisStep.active = thisStep == step;

            if (thisStep.active && $scope.onFn[step.id] && angular.isFunction($scope.onFn[step.id])) {
              $scope.onFn[step.id](thisStep, step);
            }
          });
        };

        $scope.isStepBlocked = function (step) {
          return !step.active && !step.allow;
        };

        //

        $scope.getListItemClasses = function (step) {
          var classes = [];

          if (step.active) {
            classes.push('active');
          }

          if (step.complete === true) {
            classes.push('complete');
          } else if (step.complete === false) {
            classes.push('error');
          } else {
            classes.push('incomplete');
          }

          if ($scope.isStepBlocked(step)) {
            classes.push('blocked');
          }

          return classes;
        };

        //

        transclude($scope, function ($clone) {
          $element.find('[wizard-steps]').append($clone);
        });
      };
    };

    return {
      restrict: 'E',
      templateUrl: 'wizard.html',
      scope: {
        retrieve: '&',
        update: '&'
      },
      transclude: true,
      replace: true,
      compile: compile
    };
  }
]);
