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
      integer(kind = kint) :: num_pvr = 0
!
!
!>  Structure for field parameter for PVR
!    file_params(i_pvr)%
      type(pvr_output_parameter), allocatable, save :: file_params(:)
!
!>  Structure for field parameter for PVR
      type(pvr_field_parameter), allocatable, save :: fld_params(:)
!
!>  Structure for view parameteres
      type(pvr_view_parameter), allocatable, save :: view_params(:)
!
!>  Structure for PVR colormap
      type(pvr_colormap_parameter), allocatable, save :: color_params(:)
!>  Structure for PVR colormap
      type(pvr_colorbar_parameter), allocatable, save :: cbar_params(:)
!
!
!
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
      subroutine allocate_components_4_pvr
!
      integer(kind = kint) :: i_pvr
!
!
      allocate(file_params(num_pvr))
      allocate(fld_params(num_pvr))
      allocate(view_params(num_pvr))
      allocate(color_params(num_pvr))
      allocate(cbar_params(num_pvr))
      do i_pvr = 1, num_pvr
        call reset_pvr_view_parameteres(view_params(i_pvr))
      end do
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
      do i_pvr = 1, num_pvr
        call dealloc_pvr_element_group(fld_params(i_pvr))
        call dealloc_pvr_color_parameteres(color_params(i_pvr))
      end do
      deallocate(file_params, fld_params, view_params)
      deallocate(color_params, cbar_params)
!
      end subroutine deallocate_file_name_pvr
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_pvr
