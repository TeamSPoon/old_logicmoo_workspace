<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<!--
 "Simple" GUI wrapper for the JSMC.
 (c) 2010, The Two Towers MUD. All rights reserved.
-->
<html lang="en-US" xml:lang="en-US" xmlns="http://www.w3.org/1999/xhtml" xmlns:fb="http://www.facebook.com/2008/fbml">

<head>
        <link rel="icon" href="../../images/t2t.ico" type="/image/x-icon" />
        <link rel="shortcut icon" href="../../images/t2t.ico" type="/image/x-icon" />

        <meta http-equiv="Content-Type" content="text/html"; charset="windows-1252">
        <META NAME="Author" CONTENT="logicmoo.org">
        <meta name="Description" content="The Two Towers LP MUD javascript web client">
        <meta name="Keywords" content="the two towers, mud, tolkien, mmo, mmorpg, rpg">

        <title>The Two Towers - Web Client</title>
        
        <!-- CSS -->
        <link href="jquery-ui-1.8.1.custom.css" rel="stylesheet" type="text/css"/>
        <link href="simple.css" rel="stylesheet" type="text/css"/>
        <link href="../libs/farbtastic/farbtastic.css" rel="stylesheet" type="text/css"/>
                
        <!-- Libraries -->
        <script type="text/javascript" src="../libs/jquery/jquery-1.4.2.min.js"></script>
        <script type="text/javascript" src="../libs/jquery-ui/js/jquery-ui-1.8.1.custom.min.js"></script>      
                <script language="JavaScript">var javascriptVersion1_1 = false;</script>
                <script language="JavaScript1.1">javascriptVersion1_1 = true;</script> 
                <script type="text/javascript" src="../libs/apple-plugin-detect.pack.js"></script>

        <!-- JS -->
        <script type="text/javascript" src="../common/generic.pack.js"></script>
        <script type="text/javascript" src="simple.pack.js"></script>

        <script type="text/javascript" language="JavaScript">
                var client_ready = false, dom_ready = false;
                var image_list = ["../images/overlay.png","../images/jquery-ui/ui-icons_dddddd_256x240.png","../images/jquery-ui/ui-icons_888888_256x240_border.png","../images/jquery-ui/ui-bg_highlight-soft_75_ffe45c_1x100.png","../images/jquery-ui/ui-icons_ff0000_256x240_border.png","../images/jquery-ui/ui-bg_glass_65_ffffff_1x400.png","../images/jquery-ui/ui-bg_flat_10_000000_40x100.png","../images/jquery-ui/ui-bg_glass_100_f6f6f6_1x400.png","../images/jquery-ui/ui-bg_glass_100_fdf5ce_1x400.png","../images/jquery-ui/ui-bg_gloss-wave_35_f6a828_500x100.png","../images/jquery-ui/ui-bg_diagonals-thick_20_666666_40x40.png","../images/jquery-ui/ui-bg_highlight-soft_100_eeeeee_1x100.png","../images/jquery-ui/ui-bg_diagonals-thick_18_b81900_40x40.png","../images/jquery-ui/ui-icons_ffff00_256x240_border.png","../images/fb_login_hl.png","../images/disconnect_dis.png","../images/button_up90.png","../images/help_title.png","../images/disconnect_hl.png","../images/help_hl.png","../images/go_dis.png","../images/commandbar.jpg","../images/leftborder.png","../images/apply.png","../images/botrightborder.png","../images/close.png","../images/hide_menu.png","../images/commandsep.png","../images/rightborder.png","../images/brickbackground.png","../images/options.png","../images/apply_hl.png","../images/botlogo.png","../images/botbar.png","../images/dlg_bl.png","../images/cancel_dis.png","../images/button_up30.png","../images/fb_logout_hl.png","../images/topleftborder.png","../images/options_title.png","../images/dlg_br.png","../images/close_dis.png","../images/connect_hl.png","../images/connect.png","../images/toplogo.png","../images/help.png","../images/dlg_tr.png","../images/ok.png","../images/ajax-loader.gif","../images/side_gauges.png","../images/toprightborder.png","../images/dlg_l.png","../images/fb_logout.png","../images/farbtastic/wheel.png","../images/farbtastic/mask.png","../images/farbtastic/marker.png","../images/dlg_r.png","../images/fancyleft.png","../images/dlg_tl.png","../images/apply_dis.png","../images/go.png","../images/dlg_b.png","../images/full_screen.png","../images/ok_hl.png","../images/fancyright.png","../images/fb_login.png","../images/help_dis.png","../images/cancel.png","../images/cancel_hl.png","../images/close_hl.png","../images/options_hl.png","../images/connect_dis.png","../images/button_down90.png","../images/go_hl.png","../images/topbar.png","../images/full_screen_hl.png","../images/toolbutton.png","../images/hide_menu_hl.png","../images/middlelogo.png","../images/ok_dis.png","../images/botleftborder.png","../images/button_down30.png","../images/disconnect.png","../images/bluebackground.png","../images/options_dis.png","../images/dlg_t.png"];
                
                function client_loaded()
                {
                        client_ready = true;
                        if(dom_ready && window.startup)
                                window.startup(); 
                }
                
                        if($.browser.msie && $.browser.version < 7.0)
                                $(function() {
                                        window.compatibility_error("old_ie");
                                });
                        else if(!detectFlash())
                                $(function() {
                                        window.compatibility_error("no_flash");
                                });
                        else $(function() 
                                { 
                                        dom_ready = true;
                                        if(client_ready && window.startup)
                                                window.startup(); 
                                });
        </script>
        
</head>

<body>

<noscript>
<div style="position:absolute; width: 100%, height:100%; text-align:center;" >
        <table style="width: 100%"><tbody>
        <tr>
                <td align="center">
                <span style="width: 100%, height: 100%; padding: 20px; border:2px solid #f00; background-color:#eee; display:block" border=0 cellpadding=0 cellspacing=0>
                The Two Towers web client requires Javascript.<br>To play The Two Towers, enable Javascript in your browser and reload this page. 
                </span>
                </td>
        </tr>
        </tbody></table>
</div>
</noscript>

<div id="error" style="visibility:hidden;z-index:3;position:absolute; text-align:center;">
        <table style="width: 100%, height: 100%; padding: 20px; border:2px solid #f00; background-color:#eee" border=0 cellpadding=0 cellspacing=0><tbody>
        <tr>
                <td align="center" id="error_content">
                </td>
        </tr>
        </tbody></table>
</div>

<div id="ProgressBar" style="visibility:hidden;z-index:2;position:absolute; width: 640px; height: 400px; text-align:center" >
        <table  style="width: 640px; height: 400px; border:2px solid #000;background-image: url('../images/bluebackground.png');" border=0 cellpadding=0 cellspacing=0><tbody>
        <tr>
                <td align="center">
                        <div style="width: 640px">
                                <img src="../images/middlelogo.png"/>
                        </div>
                        <div id="progress"></div>
                </td>
        </tbody></table>
</div>

<div id="MainContent" style="visibility:hidden;position:absolute;left:0px;top:0px;">
    <div id="header" style="width: 100%;" ><!-- begin header -->
                        <table style="width: 100%" cellpadding=0 cellspacing=0 border=0 ><tbody>
                                <tr>
                                        <td style="width: 142px"><img src="../images/topleftborder.png"/></td>
                                        <td style="width: 100%; text-align:center"><img src="../images/toplogo.png"/></td>
                                        <td style="width: 142px"><img src="../images/toprightborder.png"/></td>
                                </tr>
                        </tbody></table>
    </div><!-- end header -->
    <div id="wrapper1"><!-- sets background to white and creates full length leftcol-->
        
        <div id="wrapper2"><!-- sets background to white and creates full length rightcol-->
        
                <div id="main"><!-- begin main content area -->
                                
                        <div id="leftcol"><!-- begin leftcol -->
                        </div><!-- end leftcol -->
                                
                        <div id="rightcol"><!-- begin rightcol -->
                        </div><!-- end righttcol -->
                        
                        <div id="centercol"><!-- begin centercol -->
                                <div id="lower" class="frameborder">
                                    <iframe id="main_frame" src="../jsmc/client.html" class="fullfill noborder" scrolling="no" frameborder="0" onload="client_loaded()"></iframe>
                                </div>                            

							<div id="lowermenu" class="noborder fullwidth" style="background-image: url('../images/brickbackground.png'); height:64px;">
								<table style="width: 100%;" cellpadding=0 cellspacing=0 border=0 ><tbody>
											<tr>
													<td style="width: 145px;">
													   <div id="gauge_container" style="height:64px; width:145px; background-image: url('../images/fancyleft.png');"></div>
													</td>
													<td style="width: 100%;">
														<table style="width: 100%;" cellpadding=0 cellspacing=0 border=0><tbody>
																<tr>
																		<td style="width: 50%; height: 100%" valign="middle" align="right">
																		<button id="connect_btn" class="b90" title="Open connection to the game">&nbsp;</button>
																</td>
																		<td style="width: 90px; height: 100%" valign="middle">
																		<button id="disconnect_btn" class="b90" title="Close connection to the game">&nbsp;</button>
																</td>
																		<td style="width: 90px; height: 100%" valign="middle">
																		<button id="options_btn" class="b90" title="Open client options dialog">&nbsp;</button>
																</td>
																		<td style="width: 90px; height: 100%" valign="middle">
																		<button id="help_btn" class="b90" title="Open help dialog">&nbsp;</button>
																</td>
																		<td style="width: 50%; height: 100%" valign="middle">
																																			<div id="fb_login_button"></div>
																</td>
														</tr>
														</tbody></table>
												   </td>
												   <td style="width: 84px">
															<span class="noborder strip"><a class="noborder" href="http://www.topmudsites.com/cgi-bin/topmuds/rankem.cgi?id=TheTwoTowers" target="_blank"><img class="noborder" src="../images/fancyright.png"  border=0></a></span></td>
											</tr>
								</tbody></table>
							</div>                    


                        </div><!-- end centercol -->
                               
                </div><!-- end main content area -->
                                
                <div id="footer" style="width: 100%" ><!-- begin footer -->
                                <table style="width: 100%" cellpadding="0" cellspacing="0" border="0"><tbody>
                                                <tr>
                                                        <td style="width: 142px"><img src="../images/botleftborder.png"/></td>
														<td style="width:50%"><div style="width:40px">&nbsp;</div></td>
														<td style="width:259px; text-align:center"><img src="../images/botlogo.png"/></td>
														<td style="width:50%" align="right">
															<button id="menu_hide_btn" class="b40e noborder" title="Show/hide menu bar">&nbsp;</button>
														</td>
                                                        <td style="width: 142px"><img src="../images/botrightborder.png"/></td>
                                                </tr>
                                        </tbody></table>
                </div><!-- end footer -->
        
        </div><!-- end wrapper1 -->
    
    </div><!-- end wrapper2 -->
</div>  

<div id="debug_output" style="color:black;text-align:left"></div>   


<div id="options_window" title="../images/options_title.png" style="visibility:hidden" class="tab_dlg">
	<div class="noborder" id="options_tab">
		<ul>
			<li><a href="simple.php#client-options">Client options</a></li>
			<li><a href="simple.php#game-settings">Game settings</a></li>
			<li><a href="simple.php#colors-and-fonts">Colors and fonts</a></li>
		</ul>
		<div id="client-options" class="option_tab">
        <table class="subwindow_table"><tbody id="options_body">
        </tbody></table>
		</div>
		<div id="game-settings" class="option_tab">
			<div class="options_split_border">
				<div class="options_split options_splitter" sw="150px"></div>
				<div class="options_split options_split_panel left">
					<div class="options_split_panel_content" id="game_settings_categories"></div>
				</div>
				<div class="options_split options_split_panel right">
					<div class="options_split_panel_content" id="game_settings_content"></div>
				</div>
			</div>
			<div class="not_ready"></div>
		</div>
		<div id="colors-and-fonts" class="option_tab" style="font-size:0.8em">
			<div class="options_split_border">
				<div class="options_split options_splitter" sw="240px"></div>
				<div class="options_split options_split_panel left">
					<div class="options_split_panel_content" id="color_categories"></div>
				</div>
				<div class="options_split options_split_panel right">
					<div class="options_split_panel_content" id="color_content">
						<table><tbody>
							<tr class='fg'>
								<th colspan='2' align='left'>Foreground</th>
							</tr>
							<tr class='fg'>
								<td colspan='2' class='colors'></td>
							</tr>
							<tr class="bg">
								<th colspan="2" align="left">Background</th>
							</tr>
							<tr class="bg">
								<td colspan="2" class="colors"></td>
							</tr>
							<tr class="font">
								<th colspan="2" align="left">Font</th>
							</tr>
							<tr class="font font_mxp">
								<td>Font face</td><td class="font_choice" style="white-space:nowrap">face</td>
							</tr>
							<tr class="font font_mxp">
								<td>Font size</td><td class="font_choice" style="white-space:nowrap">size</td>
							</tr>
							<tr class="font">
								<td>Font attributes</td><td class="font_attribute_choice"></td>
							</tr>
							<tr>
								<th colspan="2" align="left">Sample</th>
							</tr>
							<tr>
								<td colspan="2"><pre id="color_sample"></pre></td>
							</tr>
						</tbody></table>
					</div>
				</div>
			</div>
			<div class="not_ready" style="font-size:1.25em"></div>
		</div>
	</div>
</div>

<div id="help_window" title="../images/help_title.png" style="visibility:hidden">
        <table class="subwindow_table"><tbody>
                <tr><td>
                        <table style="width:100%;"><tbody><tr>
                                <td style="width:30px; height:100%"><button id="help_prev_btn" class="b30">&nbsp;</button></td>
                                <td style="width:30px; height:100%"><button id="help_next_btn" class="b30">&nbsp;</button></td>
                                <td style="width:30px; height:100%"><button id="help_home_btn" class="b30">&nbsp;</button></td>
                                <td valign="middle" align="right" style="width:60px">Jump to:</td>
                                <td valign="middle" style="">
                                        <input id="help_input" style="width: 98%" autocomplete="off"/>
                                </td>
                                <td style="width:30px; height:100%"><button id="help_go_btn" class="b30">&nbsp;</button></td>
                        </tr></tbody></table>
                </td></tr>
                <tr>
                        <td style="border: 2px inset #444">
                                <iframe id="help_frame" style="width:100%" frameborder="0" scrolling="auto"></iframe>
                        </td>
                </tr>
        </tbody></table>
</div>
<!--ANALYTICS-->
<script src="https://www.google-analytics.com/urchin.js" type="text/javascript">
</script>
<script type="text/javascript">
_uacct = "UA-2216968-1";
urchinTracker();
</script>
<!--END ANALYTICS-->
</body>
</html>
