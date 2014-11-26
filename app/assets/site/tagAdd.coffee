'use strict'

app.directive 'tagAdd', [
  'pageService',
  (page) ->
    restrict: 'E'
    templateUrl: 'site/tagAdd.html'
    link: ($scope) ->
      form = $scope.tagAddForm

      select = (tag) -> ->
        $scope.vote(tag, true).then () -> ''

      form.search = (input) ->
        page.models.Tag.search(input).then (data) ->
            l = for tag in data
              text: tag
              select: select(tag)
            unless input in data
              l.push
                text: 'Create tag: ' + input
                select: select(input)
            l
          , (res) ->
            page.messages.addError
              body: page.constants.message('tags.auto.error', {sce: page.$sce.HTML})
              report: res
            [
              text: input
              select: select(input)
            ]

      form.submit = (input) ->
        console.log(input)
        $scope.vote(input, true).then () -> ''
]
