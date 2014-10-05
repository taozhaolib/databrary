'use strict';

app.directive('fold', [
  'pageService', 'storageService',
  function (page, storage) {
    var foldableClass = 'foldable',
      folderClass = 'folder',
      foldClass = 'fold',
      foldedClass = 'folded',
      folderAttr = '[folder]',
      foldAttr = '[folded]';

    var link = function ($scope, $element, $attrs) {
      var id = $attrs.id;
      var folded = false;
      var forget = $attrs.forget;

      function setFold(fold) {
        if ((folded = fold))
          $element.addClass(foldedClass);
        else
          $element.removeClass(foldedClass);
        if (!forget)
          storage.set('folding-' + id, folded ? '1' : '');
      }

      $scope.toggleFold = function (state) {
        setFold(state === undefined ? !folded : state);
      };

      $element.addClass(foldableClass);
      $element.find(folderAttr).addClass(folderClass);
      $element.find(foldAttr).addClass(foldClass);

      setFold(storage.get('folding-' + id));
    };

    return {
      restrict: 'A',
      priority: 0,
      link: link
    };
  }
]);
