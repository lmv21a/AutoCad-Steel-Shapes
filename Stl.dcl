// STL.DCL – Modernized Steel Shapes Dialog (Simplified)
// Updated 2025 by G.T. Watson
// Based on original by Al Rogers (1998–2002)

stl : dialog {
  label = "Steel Shapes Library";

  : text {
    label = "Version KNA";
  }

  : row {
    children_alignment = top;

    // --- Column 1: Structural Shapes ---
    : column {
      : boxed_column {
        label = "Structural Shapes";
        : button { key = "w_shape";   label = "W-Shapes"; }
        : button { key = "m_shape";   label = "M-Shapes"; }
        : button { key = "hp_shape";  label = "HP-Shapes"; }
        : button { key = "s_shape";   label = "S-Shapes"; }
        : button { key = "c_shape";   label = "C-Shapes"; }
        : button { key = "mc_shape";  label = "MC-Shapes"; }
        : button { key = "l_shape";   label = "L-Shapes"; }
        : button { key = "wt_shape";  label = "WT-Shapes"; }
        : button { key = "mt_shape";  label = "MT-Shapes"; }
        : button { key = "st_shape";  label = "ST-Shapes"; }
      }
      : boxed_column {
        label = "Formed Shapes";
        : button { key = "form_ang_shape";   label = "Angle"; }
        : button { key = "form_chann_shape"; label = "Channel"; }
        : button { key = "form_zee_shape";   label = "Zee"; }
      }
    }

    // --- Column 2: Pipes & Tubes ---
    : column {
      : boxed_column {
        label = "Pipe";
        : button { key = "std_pipe"; label = "STD"; }
        : button { key = "xs_pipe";  label = "XS"; }
        : button { key = "xxs_pipe"; label = "XXS"; }
      }
      : boxed_column {
        label = "HSS (Tubes)";
        : button { key = "tube_round"; label = "Round"; }
        : button { key = "ts_square";  label = "Square"; }
        : button { key = "ts_rect";    label = "Rectangular"; }
      }
    }

    // --- Column 3: Metric Shapes ---
    : column {
      : boxed_column {
        label = "Metric Shapes";
        : button { key = "met_w_shape"; label = "W"; }
        : button { key = "met_r_shape"; label = "R"; }
        : button { key = "met_c_shape"; label = "C"; }
        : button { key = "met_a_shape"; label = "Angles"; }
      }
      : boxed_column {
        label = "Metric Scale";
        : toggle { key = "stl_met_scale"; label = "Convert (1/25.4)"; }
      }
    }

    // --- Column 4: Size + Info ---
    : column {
      : edit_box {
        label = "Filter:";
        key = "filter_text";
        edit_width = 20;
      }
      : list_box {
        label = "Available Sizes";
        key = "get_size";
        width = 20;
        height = 15;
      }
      : boxed_column {
        label = "Shape Dimensions";
        : row { : text {label = "Depth:";}     : text {key="depth";   width=12; fixed_width=true;} }
        : row { : text {label = "Web thk.:";} : text {key="web";     width=12; fixed_width=true;} }
        : row { : text {label = "Width:";}    : text {key="width";   width=12; fixed_width=true;} }
        : row { : text {label = "Flg. Thk.:";}: text {key="flange";  width=12; fixed_width=true;} }
        : row { : text {label = "K-Dim:";}    : text {key="k_dim";   width=12; fixed_width=true;} }
      }
    }
  }

  // --- Options Row ---
  : row {
    : column {
      : boxed_radio_row {
        label = "View";
        : radio_button { key = "2d_end";     label = "2D End"; }
        : radio_button { key = "2d_top";     label = "2D Top"; }
        : radio_button { key = "2d_side";    label = "2D Side"; }
        : radio_button { key = "3d_solid";   label = "3D Solid"; }
        : radio_button { key = "3d_surface"; label = "3D Surface"; }
      }
      : edit_box {
        label = "Length:";
        edit_width = 10;
        key = "length";
      }
    }
  }

  // --- Bottom Controls ---
  : row {
    alignment = centered;
    ok_cancel;
    : button { key = "stl_help"; label = "Help"; }
  }
}

// --- Help Dialog ---
stl_help : dialog {
  label = "Help – Steel Shapes";
  : list_box {
    key = "stl_help";
    width = 80;
    height = 25;
  }
  ok_only;
}

// --- Formed Shape Sub-Dialogs ---
stl_form_ang_dialog : dialog {
  label = "Formed Angle";
  : edit_box { label="Horiz. Leg:"; edit_width=10; key="form_ang_horiz"; }
  : edit_box { label="Vert. Leg:";  edit_width=10; key="form_ang_vert"; }
  : edit_box { label="Thickness:";  edit_width=10; key="form_ang_thk"; }
  ok_cancel;
}

stl_form_chann_dialog : dialog {
  label = "Formed Channel";
  : edit_box { label="Bottom Leg:"; edit_width=10; key="form_chann_bott"; }
  : edit_box { label="Top Leg:";    edit_width=10; key="form_chann_top"; }
  : edit_box { label="Web Height:"; edit_width=10; key="form_chann_web"; }
  : edit_box { label="Thickness:";  edit_width=10; key="form_chann_thk"; }
  ok_cancel;
}

stl_form_zee_dialog : dialog {
  label = "Formed Zee";
  : edit_box { label="Bottom Leg:"; edit_width=10; key="form_zee_bott"; }
  : edit_box { label="Top Leg:";    edit_width=10; key="form_zee_top"; }
  : edit_box { label="Web Height:"; edit_width=10; key="form_zee_web"; }
  : edit_box { label="Thickness:";  edit_width=10; key="form_zee_thk"; }
  ok_cancel;
}
