'use strict';

module.controller('NetworkPanel', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.bootPanel = function () {
      getPartyAuth();
    };

    $scope.refreshPanel = function () {
      getPartyAuth();
    };

    //

    var actionMessages = {};

    $scope.$on('$destroy', function () {
      angular.forEach(actionMessages, function (bundle) {
        page.messages.remove(bundle.message);
      });
    });

    var getPartyAuth = function () {
      $scope.party.get({parents:'all', children:'all'}).then(function (party) {
	$scope.party = party;

	if (page.auth.hasAccess(page.permission.ADMIN, party))
          angular.forEach($scope.party.children, function (party) {
            if (!party.member && !party.site) {
              if (!actionMessages[party.id]) {
                actionMessages[party.id] = {
                  party: party,
                  message: page.messages.add({
                    type: 'yellow',
                    closeable: true,
                    body: page.$compile('<span>' + page.constants.message('auth.pending.notice', party.party.name) + ' <a href="' + $scope.party.editRoute('grant') + '">Manage</a>.</span>')($scope)
                  })
                };
              }
              else {
                actionMessages[party.id].party = party;
              }
            }
          });
      }, function (res) {
	page.messages.addError({
	  body: page.constants.message('network.authquery.error'),
	  report: res,
	});
      });
    };

    //

    $scope.isAdmin = page.auth.hasAccess(page.permission.ADMIN, $scope.party);

    var userExists = function (list) {
      if (!page.auth.user) return false;
      var user = page.auth.user.id;

      /* you always exist on your own page */
      return $scope.party.id <= 0 || $scope.party.id === user ||
        list.some(function (a) {
          return a.party.id === user;
        });
    };

    $scope.canGrant = !($scope.party.institution || userExists($scope.party.parents));
    $scope.canApply = !userExists($scope.party.children);

    $scope.grant = function () {
      page.$location.url(page.models.Login.user.editRoute('grant'));
      var remove = page.events.listen(page.$rootScope, 'partyEditGrantForm-init', function (event, form, $thatScope) {
        remove();

        remove = page.events.listen($thatScope, 'authSearchForm-init', function (event, searchForm) {
          if (searchForm.principal != 'child') {
            return;
          }

          searchForm.selectFn($scope.party);
          form.scrollToFuture($scope.party);
          remove();
        });
      });
    };

    $scope.apply = function () {
      page.$location.url(page.models.Login.user.editRoute('apply'));
      var remove = page.events.listen(page.$rootScope, 'partyEditApplyForm-init', function (event, form, $thatScope) {
        remove();

        remove = page.events.listen($thatScope, 'authSearchForm-init', function (event, searchForm) {
          if (searchForm.principal == 'child') {
            return;
          }

          searchForm.selectFn($scope.party);
          form.scrollToFuture($scope.party);
          remove();
        });
      });
    };
  }
]);
