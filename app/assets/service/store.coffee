'use strict'

app.factory('Store', [
  '$sce', 'constantService', 'routerService', 'messageService',
  ($sce, constants, router, messages) -> class Store
    constructor: (@slot, asset) ->
      if asset
        @setAsset(asset)
      else
        @fillData()
      return

    setAsset: (@asset) ->
      @fillData()
      return

    fillData: ->
      @data =
        if @asset
          name: @asset.name
          classification: @asset.classification+''
        else
          classification: constants.classification.RESTRICTED+''
      return

    Object.defineProperty @prototype, 'name',
      get: ->
        return constants.message('asset.add') unless @file || @asset
        @asset?.name ? @data.name ? @file?.file.name ? constants.message('file')

    remove: ->
      messages.clear(this)
      return if @pending # sorry
      return unless confirm constants.message 'asset.remove.confirm'
      if @file
        @file.cancel()
        delete @file
        return true
      return true unless @asset
      @asset.remove().then (asset) =>
          Store.removedAsset = asset
          messages.add
            type: 'green'
            body: constants.message('asset.remove.success', @name)
            owner: this
          delete @asset
          true
        , (res) =>
          messages.addError
            type: 'red'
            body: constants.message('asset.remove.error', @name)
            report: res
            owner: this
          false

    save: ->
      return if @pending # sorry
      @pending = 1
      messages.clear(this)
      (if @file
        @data.upload = @file.uniqueIdentifier
        if @asset then @asset.replace(@data) else @slot.createAsset(@data)
      else
        @asset.save(@data)
      ).then (asset) =>
          delete @pending

          first = !@asset
          @setAsset(asset)

          messages.add
            type: 'green'
            body: constants.message('asset.' + (if @file then (if first then 'upload' else 'replace') else 'update') + '.success', @name) +
              (if @file && asset.format.transcodable then ' ' + constants.message('asset.upload.transcoding') else '')
            owner: this

          if @file
            asset.creation ?= {date: Date.now(), name: @file.file.name}
            @file.cancel()
            delete @file
            delete @progress
          true
        , (res) =>
          delete @pending
          messages.addError
            type: 'red'
            body: constants.message('asset.update.error', @name)
            report: res
            owner: this
          if @file
            @file.cancel()
            delete @file
            delete @progress
            delete @data.upload
          false

    upload: (file) ->
      messages.clear(this)
      return if @file
      file.pause()
      @file = file
      @progress = 0
      file.store = this
      
      router.http(router.controllers.AssetApi.uploadStart, @slot.volume.id,
          filename: file.name
          size: file.size
        ).then (res) ->
          file.uniqueIdentifier = res.data
          file.resume()
          true
        , (res) =>
          messages.addError
            type: 'red'
            body: constants.message('asset.upload.rejected', {sce:$sce.HTML}, @name)
            report: res
            owner: this
          file.cancel()
          delete @file
          delete @progress
          false

    @restore: (slot) =>
      return unless @removedAsset?.volume.id == slot.volume.id
      messages.clear(this)
      @removedAsset.link(slot).then (a) =>
          delete @removedAsset
          a
        , (res) ->
          messages.addError
            type: 'red'
            body: constants.message('asset.update.error', '[removed file]')
            report: res
            owner: this
          return

    excerptOptions: () ->
      l = {}
      l[0] = constants.classification[@data.classification]
      for c, i in constants.classification when i > @data.classification
        l[i] = c
      l

    # callbacks for ng-flow:

    @fileSuccess = (file) ->
      file.store.progress = 1
      file.store.save()
      return

    @fileProgress = (file) ->
      file.store.progress = file.progress()
      return

    @flowOptions =
      target: router.controllers.AssetApi.uploadChunk().url
      method: 'octet'
      chunkSize: 4194304
      simultaneousUploads: 3
      testChunks: false
      chunkRetryInterval: 5000
      permanentErrors: [400, 403, 404, 409, 415, 500, 501]
      progressCallbacksInterval: 500
      prioritizeFirstAndLastChunk: true
])
