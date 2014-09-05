'use strict';

module.controller('partyEditView', [
  '$scope', 'party', 'pageService', function ($scope, party, page) {
    page.display.title = page.constants.message('page.title.party.edit');

    page.display.toolbarLinks = [
      {
        type: 'yellow',
        html: page.constants.message('party.view'),
        url: party.route,
      }
    ];

    $scope.party = party;

    //

    var updateQuery = false;

    $scope.$watch(function () {
      return page.$location.search().page;
    }, function (val, old) {
      if (!updateQuery) {
        updateQuery = true;
      }

      if (val && val !== old) {
        for (var step in $scope.wizard.steps) {
          if ($scope.wizard.steps.hasOwnProperty(step) && $scope.wizard.steps[step].id.indexOf(val) > -1) {
            $scope.wizard.activateStep($scope.wizard.steps[step]);
            break;
          }
        }
      }
    });

    //

    $scope.wizard = {};

    $scope.retrieveWizard = function (wizard) {
      $scope.wizard = wizard;
      $scope.wizard.activateFn = activateFn;
    };

    var activateFn = function (step) {
      if (updateQuery) {
        page.$location.search('page', step.id.split('-').pop());
      }
      //to avoid bug where "float" elements fixed to top of page at lower scrolls are already at top
      if (step.scrollFn){
	page.$timeout(step.scrollFn);
      }
    };

    $scope.updateWizard = function () {
      if ($scope.wizard.newStep) {
        $scope.wizard.newStep.complete = false;
        $scope.wizard.newStep.allow = true;

        if (page.$location.search().page && $scope.wizard.newStep.id.indexOf(page.$location.search().page) > -1) {
          $scope.wizard.activateStep($scope.wizard.newStep);
        } else if ($scope.wizard.newStep.id === 'party-edit-profile') {
          $scope.wizard.activateStep($scope.wizard.newStep);
        }

        if (angular.isFunction($scope.prepareStep[$scope.wizard.newStep.id])) {
          $scope.prepareStep[$scope.wizard.newStep.id]($scope.wizard.newStep);
        }
      }

      angular.forEach($scope.wizard.steps, function (step) {
        if (angular.isFunction($scope.updateStep[step.id])) {
          $scope.updateStep[step.id](step);
        }
      });
    };

    //

    var forms = {
      profile: undefined,
      account: undefined,
    };

    page.display.navigationFn = function (event, val) {
      if (!party || val.contains('/party/' + party.id + '/edit')) {
        return;
      }

      for (var id in forms) {
        if (forms.hasOwnProperty(id) && forms[id] && forms[id].form && forms[id].form.$dirty) {
          return false;
        }
      }

      return true;
    };

    $scope.$watch(function () {
      angular.forEach(forms, function (form) {
        if (!form || !form.form) {
          return;
        }

        if (form.form.$invalid) {
          form.step.complete = false;
        } else if (form.form.$dirty) {
          form.step.complete = undefined;
        } else if (form.step.allow) {
          form.step.complete = true;
        }
      });
    });

    //

    $scope.prepareStep = {
      'party-edit-profile': function (step) {
        step.enable = true;

        forms.profile = {
          step: step,
          form: step.partyEditProfileForm,
        };
        forms.profile.form.init(party);
      },

      'party-edit-account': function (step) {
        step.enable = page.models.Login.user.id === party.id || (page.models.Login.user.superuser && !party.institution);

        forms.account = {
          step: step,
          form: step.partyEditAccountForm,
        };
        forms.account.form.init(party);
      },

      'party-edit-apply': function (step) {
        step.enable = page.models.Login.checkAccess(page.permission.ADMIN, party);

        forms.apply = {
          step: step,
          form: step.partyEditApplyForm,
        };
        forms.apply.form.init(party, party.parents);
      },

      'party-edit-grant': function (step) {
        step.enable = page.models.Login.checkAccess(page.permission.ADMIN, party);

        forms.grant = {
          step: step,
          form: step.partyEditGrantForm,
        };
        forms.grant.form.init(party, party.children);
      },
    };

    //

    $scope.updateStep = {
      'party-edit-profile': function () {
        forms.profile.form.init(party);
      },

      'party-edit-account': function () {
        forms.account.form.init(party);
      },

      'party-edit-apply': function () {
        forms.apply.form.init(party, party.parents);
      },

      'party-edit-grant': function () {
        forms.grant.form.init(party, party.children);
      },
    };
  }
]);
