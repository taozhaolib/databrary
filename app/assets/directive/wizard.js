'use strict';

module.directive('wizard', [
  function () {
    var compile = function ($element, $attrs, transclude) {
      //pre-conditions
      //  $element: has wizard steps as children
      //  $attrs: confirm-nav indicates that user can't leave a dirty subpage. will be prompted to discard

      return function ($scope, $element) {
        if (angular.isFunction($scope.retrieve())) {
          $scope.retrieve()($scope);
        }

        //

        $scope.steps = [];
        $scope.stepsList = {};
        $scope.newStep = undefined;

        $scope.activateFn = $scope.activateFn || undefined;

        $scope.addStep = function (step) {
          $scope.steps.push(step);
          $scope.stepsList[step.id] = step;
          $scope.newStep = step;

          if (angular.isFunction($scope.update())) {
            $scope.update()();
          }
        };

	$scope.getActiveStep = function() {
	  for(var i in $scope.steps){
	    if($scope.steps[i].active){
	      return $scope.steps[i];
	    }
	  }
	};

        $scope.activateStep = function (newStep) {
          if ($scope.isStepBlocked(newStep)) {
            return;
          }

	  var curStep = $scope.getActiveStep();

	  //only if confirmNav is set and if we already have a current step (ie - this is not the initial load of this wizard)
	  //NOTE: this requires underlying steps to have
	  // 1) main form named as: camelCase(step.id)+'Form' (eg: volume-edit-materials --> volumeEditMaterialsForm). fallback: curStep.form
	  // 2) have a resetAll w/ conf
	  if($attrs.confirmNav && curStep && curStep != newStep){
	    var curStepForm = curStep[$.camelCase(curStep.id)+'Form'] || curStep.form;
	    if(curStepForm.$dirty && !curStepForm.resetAll()){
	      return;
	    }
	  }

          if (angular.isFunction($scope.activateFn)) {
            $scope.activateFn(newStep, $scope);
          }

	  if (curStep) curStep.active = false;
	  if (newStep) newStep.active = true;
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
