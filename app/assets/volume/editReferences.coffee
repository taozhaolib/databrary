'use strict'

app.directive 'volumeEditReferencesForm', [
  'pageService',
  (page) ->
    restrict: 'E',
    templateUrl: 'volume/editReferences.html',
    link: ($scope) ->
      volume = $scope.volume
      form = $scope.volumeEditReferencesForm

      form.data = volume.references.map (ref) ->
        head: ref.head
        url: ref.url

      blank = () ->
        form.data.push
          head: ''
          url: ''
      blank()

      form.change = () ->
        if form.data[form.data.length-1].url != ''
          blank()

      form.remove = (ref) ->
        ref.removed = true
        ref.head = ''
        ref.url = ''
        form.$setDirty()

      form.save = () ->
        data = form.data.filter (ref) ->
          !ref.removed && (ref.head != '' || ref.url != '')
        volume.save({references: data}).then(() ->
            form.validator.server {}
            form.data = data
            blank()

            form.messages.add
              type: 'green'
              countdown: 3000
              body: page.constants.message('volume.edit.success')

            form.$setPristine()
          , (res) ->
            form.validator.server res
          )

      form.scrollFn = page.display.makeFloatScrollFn($('.ver-float'), $('.ver-float-floater'), 24*2.5)
      page.$w.scroll(form.scrollFn)
]
