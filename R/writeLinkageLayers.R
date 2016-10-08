writeLinkageLayers <- function(){

	r <- '\t\t\t<div id="control_layers_container" class="control_layers_container" >
				<div id="control_layers_header" class="control_layers_header" >
					<a id="control_layers_click" class="control_layers_click" onClick="javascript:show_hide_control(this);" >&#9660;</a>
					<a class="control_layers_text">Layers</a>
				</div>
				<div class="control_layer_subheader" >
					<div class="control_layer_subheader_label" >Opacity</div>
					<div style="margin:0px 0px 0px 55px;" >Layer names</div>
				</div>
				<div id="control_layers_main" class="control_layers_main" >
					<div class="control_layer_container" >
						<div class="control_layer_header" >
							<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
							<a class="control_layer_level1"></a>
							<a class="control_layer_text_nochild">Joints</a>
						</div>
					</div>
					<div class="control_layer_container" >
						<div class="control_layer_header" >
							<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
							<a class="control_layer_level1"></a>
							<a class="control_layer_text_nochild">Points</a>
						</div>
					</div>
					<div class="control_layer_container" >
						<div class="control_layer_header" >
							<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
							<a class="control_layer_level1"></a>
							<a class="control_layer_text_nochild">Joint wire frame</a>
						</div>
					</div>
					<div class="control_layer_container" >
						<div class="control_layer_header" >
							<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
							<a class="control_layer_level1"></a>
							<a class="control_layer_text_nochild">Point wire frame</a>
						</div>
					</div>
					<div class="control_layer_container" >
						<div class="control_layer_header" >
							<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
							<a class="control_layer_level1"></a>
							<a class="control_layer_text_nochild">Joint constraints</a>
						</div>
					</div>
					<div class="control_layer_container" >
						<div class="control_layer_header" >
							<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
							<a class="control_layer_level1"></a>
							<a class="control_layer_text_nochild">Link coordinate systems</a>
						</div>
					</div>
					<div class="control_layer_container" >
						<div class="control_layer_header" >
							<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
							<a class="control_layer_level1"></a>
							<a class="control_layer_layer_vis" onClick="javascript:show_hide_control(this);" >&#9658;</a>
							<a class="control_layer_text">Coordinate box</a>
						</div>
						<div class="control_layer_main" style="display:none;" >
							<div class="control_layer_container" >
								<div class="control_layer_header" >
									<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=0 /></div>
									<a class="control_layer_level2"></a>
									<a class="control_layer_text_nochild">Bounding panel fill</a>
								</div>
							</div>
							<div class="control_layer_container" >
								<div class="control_layer_header" >
									<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=40 /></div>
									<a class="control_layer_level2"></a>
									<a class="control_layer_text_nochild">Bounding panel outline</a>
								</div>
							</div>
							<div class="control_layer_container" >
								<div class="control_layer_header" >
									<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=10 /></div>
									<a class="control_layer_level2"></a>
									<a class="control_layer_text_nochild">Grid</a>
								</div>
							</div>
							<div class="control_layer_container" >
								<div class="control_layer_header" >
									<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=50 /></div>
									<a class="control_layer_level2"></a>
									<a class="control_layer_text_nochild">Ticks</a>
								</div>
							</div>
							<div class="control_layer_container" >
								<div class="control_layer_header" >
									<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
									<a class="control_layer_level2"></a>
									<a class="control_layer_text_nochild">Tick labels</a>
								</div>
							</div>
							<div class="control_layer_container" >
								<div class="control_layer_header" >
									<div class="control_layer_shape_vis_range" ><input type="range" class="control_layer_shape_vis_range" oninput="layer_visibility(this);" value=100 /></div>
									<a class="control_layer_level2"></a>
									<a class="control_layer_text_nochild">Axis labels</a>
								</div>
							</div>
						</div>
					</div>
				</div>
			</div>
			'

	r
}
