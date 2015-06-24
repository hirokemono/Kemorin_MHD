!
!      module m_control_params_4_pvr
!
!        programmed by H.Matsui on May. 2006
!
      module m_control_params_4_pvr
!
      use m_precision
      use t_control_params_4_pvr
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
!>  Structure for view parameteres
      type(pvr_view_parameter), allocatable, save :: view_params(:)
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
      integer(kind = kint) :: i_pvr
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
      allocate(view_params(num_pvr))
      do i_pvr = 1, num_pvr
        call reset_pvr_view_parameteres(view_params(i_pvr))
      end do
!
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
      iflag_pvr_lights = 0
      pvr_lighting_real = 0.0d0
      xyz_pvr_lights =    0.0d0
!
      id_pvr_color = 0
      pvr_datamap_param = 0.0d0
      pvr_opacity_param = 0.0d0
      pvr_max_opacity = 0.0d0
!
      cbar_range = 0.0d0
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
      deallocate(view_params)
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
