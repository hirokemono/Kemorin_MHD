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
!>  Structure for PVR colormap
!    color_params(i_pvr)%
      type(pvr_colormap_parameter), allocatable, save :: color_params(:)
!
!
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
      id_pvr_file_type = 0
      id_pvr_transparent = 0
!
      nele_grp_area_pvr =   0
      istack_grp_area_pvr = 0
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
      allocate(color_params(num_pvr))
      do i_pvr = 1, num_pvr
        call reset_pvr_view_parameteres(view_params(i_pvr))
      end do
!
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
      cbar_range = 0.0d0
!
      end subroutine allocate_components_4_pvr
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_file_name_pvr
!
      integer(kind = kint) :: i_pvr
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
!
      deallocate(iflag_pvr_colorbar, iflag_pvr_cbar_nums)
      deallocate(iflag_pvr_zero_mark, ntick_pvr_colorbar, iscale_font)
!
      do i_pvr = 1, num_pvr
        call dealloc_pvr_color_parameteres(color_params(i_pvr))
      end do
      deallocate(view_params, color_params)
!
      end subroutine deallocate_file_name_pvr
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_pvr
