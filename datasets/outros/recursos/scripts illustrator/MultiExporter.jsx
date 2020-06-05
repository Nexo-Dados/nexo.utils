// MultiExporter.js
// Version 0.1
// Version 0.2 Adds PNG and EPS exports
// Version 0.3 Adds support for exporting at different resolutions
//
// Exports Illustrator artboards and/or layers as individual PNG or PDF files
// 
// Copyright 2011 Matthew Ericson
// Comments or suggestions to mericson@ericson.net


var docRef = app.activeDocument;    

var nyt_png_exporter = {
    
    png_info_lyr:   null,
    
    prefix:         null,
    suffix:         null,
    base_path:      null,
    transparency:   false,
    
    format:         null,
    export_type:    null,
    
    dlg:            null,
    prefs_xml:      null,
    
    num_to_export:  0,
    
    num_layers:     0,
    num_layers_to_export: 0, 
    
    num_artboards:  0,
    num_artboards_to_export: 0,
    
    init: function() {
		
	// figure out if there is stuff to process
	this.num_layers = docRef.layers.length;
	this.num_artboards = docRef.artboards.length;
	
	this.num_layers_to_export = this.get_num_layers_to_export();
	this.num_artboards_to_export = this.get_num_artboards_to_export();
	
	var parse_success = this.load_prefs();    
	
	if (parse_success) {
	    this.show_dialog();
	}
    },
    
    load_prefs: function() {
	
	var parse_success = false;
	
	// find existing layers or add new one
	try {
	    this.png_info_lyr = docRef.layers.getByName( 'nyt_exporter_info' );

	} catch ( e ) {
	    
	    this.png_info_lyr = docRef.layers.add();
	    this.png_info_lyr.name = 'nyt_exporter_info';
	    
	    var nyt_exporter_info_xml = this.png_info_lyr.textFrames.add();
	    
	    var saved_data = new XML( '<nyt_prefs></nyt_prefs>' );
	    saved_data.appendChild( new XML('<nyt_prefix></nyt_prefix>') );
	    saved_data.appendChild( new XML('<nyt_suffix></nyt_suffix>') );
	    saved_data.appendChild( new XML('<nyt_base_path>~/Desktop</nyt_base_path>') );
	    saved_data.appendChild( new XML('<nyt_scaling>100%</nyt_scaling>') );
	    saved_data.appendChild( new XML('<nyt_transparency>true</nyt_transparency>') );
	    saved_data.appendChild( new XML('<nyt_format>PNG 24</nyt_format>') );
	    saved_data.appendChild( new XML('<nyt_export_code>artboards</nyt_export_code>') );
	    
	    nyt_exporter_info_xml.contents = saved_data.toXMLString();    
	    
	    this.png_info_lyr.printable = false;
	    this.png_info_lyr.visible = false;
	}
	
	
	// get xml out of the 1 text item on that layer and parse it
	if ( this.png_info_lyr.textFrames.length != 1 ) {
	    Window.alert( 'Please delete the nyt_exporter_info layer and try again.' );
	    
	} else {     
	    
	    try {
		this.prefs_xml          = new XML( this.png_info_lyr.textFrames[0].contents );
		this.prefix             = this.prefs_xml.nyt_prefix;
		this.suffix             = this.prefs_xml.nyt_suffix;
		this.base_path          = this.prefs_xml.nyt_base_path;
		this.scaling 		= this.prefs_xml.nyt_scaling;
		this.transparency       = this.prefs_xml.nyt_transparency == "true" ? true : false;
		this.format             = this.prefs_xml.nyt_format;
		this.export_code        = this.prefs_xml.nyt_export_code;
		
		if ( ! this.prefs_xml.nyt_scaling || this.prefs_xml.nyt_scaling == '' ) {
		   this.scaling = '100%';
		} 

		parse_success = true;
		
	    } catch ( e ) {
		Window.alert( 'Please delete the this.png_info_lyr layer and try again.' );
	    }
	    
	}
	
	return parse_success;
    },

    
    // dialog display
    show_dialog: function() {
	
	// Export dialog
	this.dlg = new Window('dialog', 'Export Artboards'); 
	
	// PANEL to hold options
	var msgPnl = this.dlg.add('panel', undefined, 'Export Artboards and/or Layers'); 

	// EXPORT TYPE ROW
	
	var typeGrp = msgPnl.add('group', undefined, '')
	typeGrp.oreintation = 'row';
	typeGrp.alignment = [ScriptUI.Alignment.LEFT, ScriptUI.Alignment.TOP]

	var typeSt = typeGrp.add('statictext', undefined, 'Export what:'); 
	typeSt.size = [ 100,20 ];    
	
	var exportTypeList = typeGrp.add('dropdownlist', undefined, [ 'Artboards (with currently visible layers)', 'Layers (on currently selected artboard)', 'Artboards + Layers' ]);

	if ( this.export_code == 'artboards' ) {
	    exportTypeList.selection = 0;   
	} else if ( this.export_code == 'layers' ) {
	    exportTypeList.selection = 1;   
	} else {
	    exportTypeList.selection = 2;   
	}

	// PREFIX GRP
	var prefixGrp = msgPnl.add('group', undefined, '')
	prefixGrp.oreintation = 'row';
	prefixGrp.alignment = [ScriptUI.Alignment.LEFT, ScriptUI.Alignment.TOP]

	var prefixSt = prefixGrp.add('statictext', undefined, 'File prefix:'); 
	prefixSt.size = [100,20]

	var prefixEt = prefixGrp.add('edittext', undefined, this.prefix); 
	prefixEt.size = [ 300,20 ];

	var prefixGrp = msgPnl.add('group', undefined, '')
	prefixGrp.oreintation = 'row';
	prefixGrp.alignment = [ScriptUI.Alignment.LEFT, ScriptUI.Alignment.TOP]

	var suffixGrp = msgPnl.add('group', undefined, '')
	suffixGrp.oreintation = 'row';
	suffixGrp.alignment = [ScriptUI.Alignment.LEFT, ScriptUI.Alignment.TOP]

	var suffixSt = suffixGrp.add('statictext', undefined, 'File suffix:'); 
	suffixSt.size = [100,20]

	var suffixEt = suffixGrp.add('edittext', undefined, this.suffix); 
	suffixEt.size = [ 300,20 ];

	var dpiGrp = msgPnl.add('group', undefined, '')
	dpiGrp.oreintation = 'row';
	dpiGrp.alignment = [ScriptUI.Alignment.LEFT, ScriptUI.Alignment.TOP]

	var dpiSt = dpiGrp.add('statictext', undefined, 'Scaling:'); 
	dpiSt.size = [100,20]

	var dpiEt = dpiGrp.add('edittext', undefined, this.scaling); 
	dpiEt.size = [ 100,20 ];

	var dpiSt2 = dpiGrp.add('statictext', undefined, '(Normally 100%; Use 200% for Retina dislay exports)'); 
	dpiSt2.size = [300,20]

	// DIR GROUP
	var dirGrp = msgPnl.add( 'group', undefined, '') 
	dirGrp.orientation = 'row'
	dirGrp.alignment = [ScriptUI.Alignment.LEFT, ScriptUI.Alignment.TOP]
	
	var dirSt = dirGrp.add('statictext', undefined, 'Output directory:'); 
	dirSt.size = [ 100,20 ];

	var dirEt = dirGrp.add('edittext', undefined, this.base_path); 
	dirEt.size = [ 300,20 ];

	var chooseBtn = dirGrp.add('button', undefined, 'Choose ...' );
	chooseBtn.onClick = function() { dirEt.text = Folder.selectDialog(); }

	// TRANSPARENCY AND FORMAT ROW
	var transPnl = msgPnl.add('group', undefined, ''); 
	transPnl.orientation = 'row'
	transPnl.alignment = [ScriptUI.Alignment.LEFT, ScriptUI.Alignment.TOP]
	
	var formatSt = transPnl.add('statictext', undefined, 'Export format:'); 
	formatSt.size = [ 100,20 ];    
	
	var formatList = transPnl.add('dropdownlist', undefined, [ 'PNG 8', 'PNG 24', 'PDF', 'JPG', 'EPS' ]);
	
	formatList.selection = 1;
	for ( var i=0; i < formatList.items.length; i++ ) {
            if ( nyt_png_exporter.format == formatList.items[i].text ) {
		formatList.selection = i;
            }
	}
	
	transPnl.transparentChk = transPnl.add('checkbox', undefined, 'Transparency');
	transPnl.transparentChk.value = this.transparency;

	var progBar = msgPnl.add( 'progressbar', undefined, 0, 100 );
	progBar.size = [400,10]

	var progLabel = msgPnl.add('statictext', undefined, '...' ); 
	progLabel.size = [ 400,20 ];

	var btnPnl = this.dlg.add('group', undefined, ''); 
	btnPnl.orientation = 'row'

	btnPnl.cancelBtn = btnPnl.add('button', undefined, 'Cancel', {name:'cancel'});
	btnPnl.cancelBtn.onClick = function() { nyt_png_exporter.dlg.close() };

	// OK button
	btnPnl.okBtn = btnPnl.add('button', undefined, 'Export', {name:'ok'});
	btnPnl.okBtn.onClick = function() { 
            
            nyt_png_exporter.prefix       = prefixEt.text; 
            nyt_png_exporter.suffix       = suffixEt.text; 
            nyt_png_exporter.base_path    = dirEt.text;   
            nyt_png_exporter.transparency = transPnl.transparentChk.value; 
            nyt_png_exporter.format       = formatList.selection.text;
            nyt_png_exporter.export_type  = exportTypeList.selection.text;
	    nyt_png_exporter.scaling      = parseFloat( dpiEt.text.replace( /\% /, '' ));   

            if ( nyt_png_exporter.export_type.match( /\+/ )) {
		nyt_png_exporter.export_code = 'both';        
            } else if ( nyt_png_exporter.export_type.match( /^Art/  )) {
		nyt_png_exporter.export_code = 'artboards';        
            } else {
		nyt_png_exporter.export_code = 'layers';        
            }      
	    
            nyt_png_exporter.run_export();   
	};
	
	// Export type handler
	exportTypeList.onChange = function() { 
	    
            nyt_png_exporter.export_type  = exportTypeList.selection.text;
	    
            if ( nyt_png_exporter.export_type.match( /\+/ )) {
		nyt_png_exporter.export_code = 'both';        
            } else if ( nyt_png_exporter.export_type.match( /^Art/  )) {
		nyt_png_exporter.export_code = 'artboards';        
            } else {
		nyt_png_exporter.export_code = 'layers';        
            }        
            
            nyt_png_exporter.update_export_desc( progLabel );
	};

	nyt_png_exporter.update_export_desc( progLabel );
	
	this.dlg.progLabel = progLabel;
	this.dlg.progBar = progBar;
	
	this.dlg.show();
    },

    
    update_export_desc: function ( progLabel ) {
	
	if ( this.export_code == 'artboards' ) {
	    progLabel.text = 'Will export ' + this.num_artboards_to_export + ' of ' + this.num_artboards + ' artboards';
	    this.num_to_export = this.num_artboards_to_export;
	    
	} else if ( this.export_code == 'layers' ) {    
	    var current_name = docRef.artboards[ docRef.artboards.getActiveArtboardIndex() ].name; 
	    progLabel.text = 'Will export ' + this.num_layers_to_export + ' of ' + this.num_layers+ ' layers on artboard "' + current_name +'"' ;        
	    this.num_to_export = this.num_layers_to_export;
	    
	} else {
	    this.num_to_export = this.num_layers_to_export * this.num_layers_to_export ;
	    progLabel.text = 'Will export ' + this.num_to_export + ' files (' + this.num_layers_to_export + ' layers * ' + this.num_artboards_to_export + ' artboards)';        

	}
    },

    
    // run_export function. does the dirty work
    run_export: function() {

	var num_exported = 0;
	var options;
	
	if ( this.format =='PNG 8' ) {
	    options = new ExportOptionsPNG8();
	    options.antiAliasing = true;
	    options.transparency = this.transparency; 
	    options.artBoardClipping = true;
	    options.horizontalScale = this.scaling;
	    options.verticalScale = this.scaling;		    
            
	} else if ( this.format == 'PNG 24' ) {
	    options = new ExportOptionsPNG24();
	    options.antiAliasing = true;
	    options.transparency = this.transparency; 
	    options.artBoardClipping = true;
	    options.horizontalScale = this.scaling;
	    options.verticalScale = this.scaling;		    
	    
	} else if ( this.format == 'PDF' ) {
            options = new PDFSaveOptions();
            options.compatibility = PDFCompatibility.ACROBAT5;
            options.generateThumbnails = true;
            options.preserveEditability = false;
	    
	} else if ( this.format == 'JPG' ) {
	    options = new ExportOptionsJPEG();
	    options.antiAliasing = true;
	    options.artBoardClipping = true;
	    options.horizontalScale = this.scaling;
	    options.verticalScale = this.scaling;		    
	    
	} else if ( this.format == 'EPS' ) {
            options = new EPSSaveOptions();
            options.embedLinkedFiles = true;
            options.includeDocumentThumbnails = true;
	    options.saveMultipleArtboards = true;
	}
		    
	var starting_artboard = 0;
	var num_artboards =  docRef.artboards.length;
	
	if ( this.export_code == 'layers' ) {
	    starting_artboard = docRef.artboards.getActiveArtboardIndex();
	    num_artboards = starting_artboard + 1; 
	}
	
	for (var i = starting_artboard; i < num_artboards; i++ ) {
	    
            var artboardName = docRef.artboards[i].name;
            starting_artboard = docRef.artboards.setActiveArtboardIndex(i);
	    
            // Process this artbarod if we're exporting only a single one (layers mode) or if it doesn't have generic name or minus
            if ( this.export_code == 'layers' || ! ( artboardName.match(  /^artboard/i ) || artboardName.match( /^\-/ ) )) {
		
		// if exporting artboard by artboard, export layers as is
		if ( this.export_code == 'artboards' ) {
                    
		    var base_filename = this.base_path + "/" + this.prefix + artboardName + this.suffix 

		    if ( this.format.match( /^PNG/ )) {                
			var destFile = new File( base_filename + '.png' );   
			var export_type = this.format == 'PNG 8' ? ExportType.PNG8 : ExportType.PNG24;
			docRef.exportFile(destFile, export_type , options);
			
	            } else if ( this.format.match( /^JPG/ )) {
	                var destFile = new File( base_filename + '.jpg' );   
	                var export_type = ExportType.JPEG;
	                docRef.exportFile(destFile, export_type , options);
			
	            } else if ( this.format.match( /^EPS/ )) {

			// dumb. specify a filename, saveAs saves as something else
			// so after we save it, we have to rename it to what we want.
			var eps_real_filename = base_filename + '_' + artboardName + '.eps'
			
	                var destFile = new File( base_filename + '.eps' );   
	                options.artboardRange = (i+1).toString();
	                docRef.saveAs( destFile, options );
			
			var epsFile = new File ( eps_real_filename );
			if ( epsFile.exists ) {
			    epsFile.copy( base_filename + '.eps'  );
                            epsFile.remove();
			}
			
		    } else if ( this.format.match( /^PDF/ )) {
			var destFile = new File( base_filename + '.pdf' );   
			options.artboardRange = (i+1).toString();
			docRef.saveAs( destFile, options )
		    }       
                    
		    // export layers as individual files
		} else if ( this.export_code == 'layers' || this.export_code = 'both' ) {
		    
		    for ( var j=0; j < docRef.layers.length; j++ ) {
			
			this.hide_all_layers();
			
			var layer = docRef.layers[j];
			var lyr_name = layer.name;
			
			if ( ! ( lyr_name.match( /^\+/ ) || lyr_name.match( /nyt_exporter_info/ ) || lyr_name.match( /^\-/) || lyr_name.match( /^Layer / ) )) {
			    
			    var base_filename;
			    
			    if ( this.export_code == 'layers' ) {
				base_filename = this.base_path + "/" + this.prefix + lyr_name + this.suffix 
				
			    } else if ( this.export_code == 'both' ) {
				base_filename = this.base_path + "/" + this.prefix + artboardName + '-' + lyr_name + this.suffix 

			    }
			    
			    layer.visible = true;
			    
			    if ( this.format.match( /^PNG/ )) {
				var destFile = new File( base_filename + '.png' );   
				var export_type = this.format == 'PNG 8' ? ExportType.PNG8 : ExportType.PNG24;
				docRef.exportFile(destFile, export_type , options);
				
			    } else if ( this.format.match( /^JPG/ )) {
				var destFile = new File( base_filename + '.jpg' );   
				var export_type = ExportType.JPEG;
				docRef.exportFile(destFile, export_type , options);
				
			    } else if ( this.format.match( /^EPS/ )) {
				
				var eps_real_filename = base_filename + '_' + artboardName + '.eps'
				
				var destFile = new File( base_filename + '.eps' );   
				options.artboardRange = (i+1).toString();
				docRef.saveAs( destFile, options )
				
				var epsFile = new File ( eps_real_filename );
				if ( epsFile.exists ) {
				    epsFile.copy( base_filename + '.eps'  );
				    epsFile.remove();
				    
				}
				
				
			    } else if ( this.format.match( /^PDF/ )) {
				var destFile = new File( base_filename + '.pdf' );   
				options.artboardRange = (i+1).toString();
				docRef.saveAs( destFile, options )
			    }
			    
			    num_exported++;
			    
			    this.dlg.progLabel.text = 'Exported ' + num_exported + ' of ' + this.num_to_export;
			    this.dlg.progBar.value = num_exported / this.num_to_export * 100;
			    
			    this.dlg.update();        
			}
			
		    }
		}
	    }
        }   
	
	this.prefs_xml.nyt_base_path    = this.base_path;
	this.prefs_xml.nyt_scaling      = this.scaling;
	this.prefs_xml.nyt_prefix       = this.prefix;
	this.prefs_xml.nyt_suffix       = this.suffix;
	this.prefs_xml.nyt_transparency = this.transparency;
	this.prefs_xml.nyt_format       = this.format;
	this.prefs_xml.nyt_export_code  = this.export_code;
    
	this.png_info_lyr.textFrames[0].contents = this.prefs_xml.toXMLString();
	this.dlg.close();

	//end run_export
    },
    
    hide_all_layers: function() {
	var export_count = 0;
	
	var n = docRef.layers.length;
	
	for(var i=0; i<n; ++i) {
	    
	    layer = docRef.layers[i];
	    
	    lyr_name = layer.name;
	    
	    // any layers that start with + are always turned on
	    if ( lyr_name.match( /^\+/ ) ) {
		layer.visible = true;
		
	    // any layers that start with -, have default layer name, or named "nyt_exporter_info" are skipped
	    } else if ( lyr_name.match( /nyt_exporter_info/ ) || lyr_name.match( /^Layer /) || lyr_name.match( /^\-/ ) ){
		layer.visible = false;
		
	    // everything else we should export
	    } else {
		layer.visible = false;
		export_count++;
	    }
	}
	return export_count;
    },
    
    get_num_layers_to_export: function() {

	var num_to_export = 0;
	var num_layers = docRef.layers.length;
	
	for(var i=0; i<num_layers; ++i) {
	    
	    var layer = docRef.layers[i];      
	    var lyr_name = layer.name;
	    
	    // any layers that start with + are always turned on
	    if ( lyr_name.match( /^\+/ ) ) {
		
	    } else if ( lyr_name.match( /nyt_exporter_info/ ) || lyr_name.match( /^Layer /) || lyr_name.match( /^\-/ ) ){
		
	    } else {
		num_to_export++;
	    }
	}
	
	return num_to_export;
    },
    
    get_num_artboards_to_export: function() {
	
	var num_artboards = docRef.artboards.length;
	var num_to_export = 0;
	
	for (var i = 0; i < num_artboards; i++ ) {
            
            var artboardName = docRef.artboards[i].name;
            if ( ! ( artboardName.match(  /^artboard/i ) || artboardName.match( /^\-/ ) )){
                num_to_export++;
            }
	} 
	
	return num_to_export;
	
    },
    
    
    show_all_layers: function() {
	var n = docRef.layers.length;
	for(var i=0; i<n; ++i) {
	    layer = docRef.layers[i];
	    layer.visible = true;
	}
    },
    
};

nyt_png_exporter.init();