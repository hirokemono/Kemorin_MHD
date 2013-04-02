!
!      module m_control_params_4_pvr
!
!        programmed by H.Matsui on May. 2006
!
      module m_control_params_4_pvr
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: pvr_ctl_file_code = 11
!
      integer(kind = kint) :: num_pvr = 0
      character(len = kchara), allocatable :: pvr_header(:)
!
!
      integer(kind = kint), allocatable :: id_pvr_file_type(:)
      integer(kind = kint), allocatable :: id_pvr_transparent(:)
!
      integer(kind = kint) :: ntot_grp_area_pvr
      integer(kind = kint), allocatable :: nele_grp_area_pvr(:)
      integer(kind = kint), allocatable :: istack_grp_area_pvr(:)
!
      integer(kind = kint), allocatable :: id_ele_grp_area_pvr(:)
!
!
      integer(kind = kint), allocatable :: id_pvr_output(:)
      integer(kind = kint), allocatable :: icomp_pvr_output(:)
      integer(kind = kint), allocatable :: ncomp_pvr_org(:)
      character(len = kchara), allocatable :: name_pvr_output(:)
!
!
      integer(kind = kint), allocatable :: n_pvr_pixel(:,:)
!
      real(kind = kreal), allocatable :: ortho_pvr(:,:)
!
!
!
      integer(kind = kint), allocatable :: iflag_modelview_mat(:)
      real(kind = kreal), allocatable :: modelview_mat(:,:)
      real(kind = kreal), allocatable :: modelview_inv(:,:)
!
      integer(kind = kint), allocatable :: iflag_perspective_mat(:)
      integer(kind = kint), allocatable :: iflag_stereo_pvr(:)
!
      real(kind = kreal), allocatable :: projection_mat(:,:)
      real(kind = kreal), allocatable :: projection_left(:,:)
      real(kind = kreal), allocatable :: projection_right(:,:)
!
      real(kind = kreal), allocatable :: perspective_angle(:)
      real(kind = kreal), allocatable :: perspective_xy_ratio(:)
      real(kind = kreal), allocatable :: perspective_near(:)
      real(kind = kreal), allocatable :: perspective_far(:)
!
      real(kind = kreal), allocatable :: focalLength(:)
      real(kind = kreal), allocatable :: eye_separation(:)
!
      integer(kind = kint), allocatable :: iflag_ortho_mat(:)
      real(kind = kreal), allocatable :: ortho_mat(:,:)
!
      integer(kind = kint), allocatable :: iflag_rotation_pvr(:)
      real(kind = kreal), allocatable :: rotation_pvr(:,:)
      integer(kind = kint), allocatable :: iflag_scale_fact_pvr(:)
      real(kind = kreal), allocatable :: scale_factor_pvr(:,:)
      integer(kind = kint), allocatable :: iflag_viewpt_in_view_pvr(:)
      real(kind = kreal), allocatable :: viewpt_in_viewer_pvr(:,:)
!
      integer(kind = kint), allocatable :: iflag_lookpoint_vec(:)
      real(kind = kreal), allocatable :: lookat_vec(:,:)
!
      integer(kind = kint), allocatable :: iflag_viewpoint_vec(:)
      real(kind = kreal), allocatable :: viewpoint_vec(:,:)

      integer(kind = kint), allocatable :: iflag_updir_vec(:)
      real(kind = kreal), allocatable :: up_direction_vec(:,:)
!
!
      real(kind = kreal), allocatable :: pvr_lighting_real(:,:)
!        ambient_coef(:) =  pvr_lighting_real(1,:)
!        diffuse_coef(:) =  pvr_lighting_real(2,:)
!        specular_coef(:) = pvr_lighting_real(3,:)
      integer(kind = kint), allocatable :: iflag_pvr_lights(:)
      integer(kind = kint) :: ntot_pvr_lights
      integer(kind = kint), allocatable :: num_pvr_lights(:)
      integer(kind = kint), allocatable :: istack_pvr_lights(:)
      real(kind = kreal), allocatable :: xyz_pvr_lights(:,:)
      real(kind = kreal), allocatable :: view_pvr_lights(:,:)
!
!
      integer(kind = kint), allocatable :: id_pvr_color(:,:)
!        pvr_colormap(:) =        id_pvr_color(1,:)
!        pvr_data_mapping(:) =    id_pvr_color(2,:)
!        opacity_style(:) =       find_dis_minmax(3,:)
!
      integer(kind = kint) :: ntot_pvr_datamap_pnt
      integer(kind = kint), allocatable :: num_pvr_datamap_pnt(:)
      integer(kind = kint), allocatable :: istack_pvr_datamap_pnt(:)
      real(kind = kreal), allocatable :: pvr_datamap_param(:,:)
!        pvr_datamap_data(:) =  pvr_datamap_param(1,:)
!        pvr_datamap_color(:) = pvr_datamap_param(2,:)
!
      integer(kind = kint) :: ntot_opacity_pnt
      integer(kind = kint), allocatable :: num_opacity_pnt(:)
      integer(kind = kint), allocatable :: istack_opacity_pnt(:)
      real(kind = kreal), allocatable :: pvr_opacity_param(:,:)
      real(kind = kreal), allocatable :: pvr_max_opacity(:)
!        pvr_opacity_dat_low(:) =  pvr_opacity_param(1,:)
!        pvr_opacity_dat_high(:) = pvr_opacity_param(2,:)
!        pvr_opacity_opacity(:) =  pvr_opacity_param(3,:)
!        ambient_opacity(:) = pvr_opacity_param(3,(num_opacity_pnt(:)+1))
!
!
!
      integer(kind = kint) :: iflag_rotation = 0
      integer(kind = kint) :: max_rotation =   0
      integer(kind = kint), allocatable :: iprm_pvr_rot(:,:)
!        rot_axis(:) =   iprm_pvr_rot(1,:)
!        num_frames(:) = iprm_pvr_rot(2,:)
!
!
      integer(kind = kint), allocatable :: iflag_pvr_colorbar(:)
      integer(kind = kint), allocatable :: iflag_pvr_cbar_nums(:)
      integer(kind = kint), allocatable :: iflag_pvr_zero_mark(:)
      integer(kind = kint), allocatable :: iscale_font(:)
      integer(kind = kint), allocatable :: ntick_pvr_colorbar(:)
      real(kind = kreal), allocatable :: cbar_range(:,:)
!
!
!      subroutine allocate_ctl_param_4_pvr
!      subroutine allocate_components_4_pvr
!      subroutine allocate_loght_posi_in_view
!
!      subroutine deallocate_file_name_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ctl_param_4_pvr
!
!
      allocate( pvr_header(num_pvr) )
      allocate( id_pvr_file_type(num_pvr) )
      allocate( id_pvr_transparent(num_pvr) )
!
      allocate( nele_grp_area_pvr(num_pvr) )
      allocate( istack_grp_area_pvr(0:num_pvr) )
!
      allocate(num_pvr_lights(num_pvr) )
      allocate(istack_pvr_lights(0:num_pvr) )
!
      allocate(num_pvr_datamap_pnt(num_pvr) )
      allocate(istack_pvr_datamap_pnt(0:num_pvr) )
!
      allocate(num_opacity_pnt(num_pvr) )
      allocate(istack_opacity_pnt(0:num_pvr) )
!
      id_pvr_file_type = 0
      id_pvr_transparent = 0
!
      nele_grp_area_pvr =   0
      istack_grp_area_pvr = 0
!
      num_pvr_lights =   0
      istack_pvr_lights= 0
!
      num_pvr_datamap_pnt =   0
      istack_pvr_datamap_pnt= 0
!
      num_opacity_pnt =   0
      istack_opacity_pnt= 0
!
      end subroutine allocate_ctl_param_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_components_4_pvr
!
!
      ntot_grp_area_pvr =    istack_grp_area_pvr(num_pvr)
      ntot_pvr_lights =      istack_pvr_lights(num_pvr)
      ntot_pvr_datamap_pnt = istack_pvr_datamap_pnt(num_pvr)
      ntot_opacity_pnt =     istack_opacity_pnt(num_pvr)
!
      allocate(id_pvr_output(num_pvr)    )
      allocate(icomp_pvr_output(num_pvr) )
      allocate(ncomp_pvr_org(num_pvr)    )
!
      allocate(name_pvr_output(num_pvr)  )
!
      allocate(id_ele_grp_area_pvr(ntot_grp_area_pvr))
!
!
      allocate(n_pvr_pixel(2,num_pvr)  )
!
      allocate(ortho_pvr(2,num_pvr)  )
!
      allocate(iflag_modelview_mat(num_pvr)  )
      allocate(iflag_perspective_mat(num_pvr)  )
      allocate(iflag_stereo_pvr(num_pvr)  )
      allocate(iflag_ortho_mat(num_pvr)  )
!
      allocate(iflag_lookpoint_vec(num_pvr)  )
      allocate(iflag_viewpoint_vec(num_pvr)  )
      allocate(iflag_updir_vec(num_pvr)  )
!
      allocate(iflag_rotation_pvr(num_pvr)  )
      allocate(iflag_scale_fact_pvr(num_pvr)  )
      allocate(iflag_viewpt_in_view_pvr(num_pvr)  )
!
      allocate(modelview_mat(16,num_pvr)     )
      allocate(modelview_inv(16,num_pvr)     )
!
      allocate(projection_mat(16,num_pvr)     )
      allocate(projection_left(16,num_pvr)    )
      allocate(projection_right(16,num_pvr)   )
!
      allocate(ortho_mat(3,num_pvr)     )
!
      allocate(perspective_angle(num_pvr)    )
      allocate(perspective_xy_ratio(num_pvr) )
      allocate(perspective_near(num_pvr)     )
      allocate(perspective_far(num_pvr)      )
!
      allocate(focalLength(num_pvr)     )
      allocate(eye_separation(num_pvr)  )
!
      allocate(lookat_vec(3,num_pvr)  )
      allocate(viewpoint_vec(3,num_pvr)  )
      allocate(up_direction_vec(3,num_pvr) )
!
      allocate(rotation_pvr(4,num_pvr)  )
      allocate(scale_factor_pvr(3,num_pvr) )
      allocate(viewpt_in_viewer_pvr(4,num_pvr) )
!
      allocate(iflag_pvr_lights(num_pvr) )
      allocate(pvr_lighting_real(3,num_pvr) )
      allocate(xyz_pvr_lights(3,ntot_pvr_lights) )
!
      allocate(id_pvr_color(3,num_pvr) )
      allocate(pvr_datamap_param(2,ntot_pvr_datamap_pnt) )
      allocate(pvr_opacity_param(3,ntot_opacity_pnt) )
      allocate(pvr_max_opacity(num_pvr) )
!
      allocate(iprm_pvr_rot(2,num_pvr) )
      allocate(cbar_range(2,num_pvr))
!
      allocate(iflag_pvr_colorbar(num_pvr))
      allocate(iflag_pvr_cbar_nums(num_pvr))
      allocate(iflag_pvr_zero_mark(num_pvr))
      allocate(ntick_pvr_colorbar(num_pvr))
      allocate(iscale_font(num_pvr))
      iscale_font = 0
      ntick_pvr_colorbar =   0
      iflag_pvr_colorbar =   0
      iflag_pvr_cbar_nums =  0
      iflag_pvr_zero_mark =  0
!
      id_pvr_output =    0
      icomp_pvr_output = 0
      ncomp_pvr_org =    0
!
      id_ele_grp_area_pvr = 0
!
      n_pvr_pixel = 0
!
      ortho_mat = 0.0d0
      ortho_pvr = 0.0d0
!
      modelview_mat =        0.0d0
      modelview_inv =        0.0d0
      projection_mat =       0.0d0
      projection_left =      0.0d0
      projection_right =     0.0d0
!
      perspective_angle =    0.0d0
      perspective_xy_ratio = 0.0d0
      perspective_near =     0.0d0
      perspective_far =      0.0d0
!
      focalLength =          1.0d0
      eye_separation =       0.0d0
!
      lookat_vec =       0.0d0
      viewpoint_vec =    0.0d0
      up_direction_vec = 0.0d0
!
      iflag_pvr_lights = 0
      pvr_lighting_real = 0.0d0
      xyz_pvr_lights =    0.0d0
!
      id_pvr_color = 0
      pvr_datamap_param = 0.0d0
      pvr_opacity_param = 0.0d0
      pvr_max_opacity = 0.0d0
!
      iprm_pvr_rot =  0
      cbar_range = 0.0d0
!
      rotation_pvr = 0.0d0
      scale_factor_pvr = 0.0d0
      viewpt_in_viewer_pvr = 0.0d0
!
      iflag_modelview_mat =  0
      iflag_lookpoint_vec =  0
      iflag_rotation_pvr = 0
      iflag_scale_fact_pvr = 0
      iflag_updir_vec = 0
      iflag_viewpoint_vec = 0
      iflag_viewpt_in_view_pvr = 0
      iflag_ortho_mat = 0
      iflag_perspective_mat = 0
      iflag_stereo_pvr = 0
!
      end subroutine allocate_components_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_loght_posi_in_view
!
      allocate(view_pvr_lights(3,ntot_pvr_lights) )
      if (ntot_pvr_lights.gt. 0) view_pvr_lights =   0.0d0
!
      end subroutine allocate_loght_posi_in_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_file_name_pvr
!
!
      deallocate(pvr_header)
      deallocate(id_pvr_file_type, id_pvr_transparent)
!
      deallocate(nele_grp_area_pvr  )
      deallocate(istack_grp_area_pvr)
!
      deallocate(id_ele_grp_area_pvr  )
!
      deallocate(id_pvr_output    )
      deallocate(icomp_pvr_output )
      deallocate(ncomp_pvr_org    )
      deallocate(name_pvr_output  )
!
!
      deallocate(n_pvr_pixel)
!
      deallocate(ortho_pvr)
!
      deallocate(iflag_ortho_mat, iflag_perspective_mat)
      deallocate(ortho_mat)
!
      deallocate(iflag_stereo_pvr)
!
      deallocate(iflag_modelview_mat, iflag_lookpoint_vec )
      deallocate(iflag_viewpoint_vec, iflag_updir_vec     )
!
      deallocate(iflag_rotation_pvr, iflag_scale_fact_pvr )
      deallocate(iflag_viewpt_in_view_pvr)
!
      deallocate(modelview_mat, modelview_inv)
      deallocate(projection_mat, projection_left, projection_right)
!
      deallocate(perspective_angle    )
      deallocate(perspective_xy_ratio )
      deallocate(perspective_near     )
      deallocate(perspective_far      )
!
      deallocate(focalLength, eye_separation)
!
      deallocate(lookat_vec   )
      deallocate(viewpoint_vec   )
      deallocate(up_direction_vec)
!
      deallocate(rotation_pvr   )
      deallocate(scale_factor_pvr)
      deallocate(viewpt_in_viewer_pvr)
!
      deallocate(iprm_pvr_rot )
!
      deallocate(iflag_pvr_colorbar, iflag_pvr_cbar_nums)
      deallocate(iflag_pvr_zero_mark, ntick_pvr_colorbar, iscale_font)
!
      deallocate(istack_opacity_pnt, num_opacity_pnt)
      deallocate(num_pvr_lights, istack_pvr_lights)
!
      deallocate(istack_pvr_datamap_pnt, num_pvr_datamap_pnt)
      deallocate(id_pvr_color, pvr_datamap_param)
      deallocate(pvr_opacity_param, pvr_max_opacity)
      deallocate(iflag_pvr_lights, pvr_lighting_real )
      deallocate(xyz_pvr_lights, view_pvr_lights)
!
      end subroutine deallocate_file_name_pvr
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_pvr
