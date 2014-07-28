'use strict';

module.directive('searchForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.searchForm;

      form.data = {
	query: page.$location.search().query
      };

      form.focus = function () {
	if (page.$location.path() !== '/search') {
	  page.$location.path('/search');
	  page.$sessionStorage.searchFocus = true;
	}
      };

      form.blur = function () {
	if (page.$location.path() === '/search') {
	  page.$sessionStorage.searchFocus = false;
	}
      };

      $scope.$watch('searchForm.data.query', function () {
	page.$location.search('query', form.data.query || undefined);
      });

      //

      page.events.talk('searchForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'searchForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
