'use strict';

module.controller('party/network', [
  '$scope', 'pageService', function ($scope, page) {
    $scope.refreshPanel = getPartyAuth;

    //

    var actionMessages = {};

    $scope.$on('$destroy', function () {
      angular.forEach(actionMessages, function (bundle) {
        page.messages.remove(bundle.message);
      });
    });

    var getPartyAuth = function () {
      if (!$scope.party.checkPermission(page.permission.ADMIN))
        return;
      angular.forEach($scope.party.children, function (party) {
        if (!party.member && !party.site) {
          if (!actionMessages[party.id]) {
            actionMessages[party.id] = {
              party: party,
              message: page.messages.add({
                type: 'yellow',
                closeable: true,
                body: page.$sce.trustAsHtml('<span>' + page.constants.message('auth.pending.notice', {sce:page.$sce.HTML}, party.party.name) + ' <a href="' + $scope.party.editRoute('grant') + '">Manage</a>.</span>')
              })
            };
          }
          else {
            actionMessages[party.id].party = party;
          }
        }
      });
    };

    //

    $scope.isAdmin = $scope.party.checkPermission(page.permission.ADMIN);

    var userExists = function (list) {
      var user = page.models.Login.user.id;

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
      var remove = page.$rootScope.$on('partyEditGrantForm-init', function (event, form) {
        form.preSelect($scope.party);
        remove();
      });
    };

    $scope.apply = function () {
      page.$location.url(page.models.Login.user.editRoute('apply'));
      var remove = page.$rootScope.$on('partyEditApplyForm-init', function (event, form) {
        form.preSelect($scope.party);
        remove();
      });
    };
  }
]);
