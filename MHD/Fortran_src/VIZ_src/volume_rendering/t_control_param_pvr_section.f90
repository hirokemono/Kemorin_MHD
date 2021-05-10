!>@file   t_control_param_pvr_section.f90
!!       module t_control_param_pvr_section
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine set_control_pvr_sections(pvr_scts_c, pvr_section_p)
!!      subroutine dealloc_pvr_sections(pvr_section_p)
!!        type(pvr_sections_ctl), intent(in) :: pvr_scts_c
!!        type(pvr_section_parameter), intent(inout) :: pvr_section_p
!!@endverbatim
!
      module t_control_param_pvr_section
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>  Structure for cross sections for volume rendering
      type pvr_section_parameter
!>        Number of sections
        integer(kind = kint) :: num_sections
!>        fiale value for isosurfaces
        real(kind = kreal), allocatable :: coefs(:,:)
!>        Opacity value for isosurfaces
        real(kind = kreal), allocatable :: sect_opacity(:)
      end type pvr_section_parameter
!
      private :: alloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_pvr_sections(pvr_scts_c, pvr_section_p)
!
      use t_control_data_pvr_sections
      use set_coefs_of_sections
      use set_control_pvr_color
      use skip_comment_f
!
      type(pvr_sections_ctl), intent(in) :: pvr_scts_c
!
      type(pvr_section_parameter), intent(inout) :: pvr_section_p
!
      integer(kind = kint) :: id_section_method, ierr, i
!
!
      pvr_section_p%num_sections = pvr_scts_c%num_pvr_sect_ctl
      call alloc_pvr_sections(pvr_section_p)
      if(pvr_section_p%num_sections .gt. 0) then
!
        do i = 1, pvr_section_p%num_sections
          call s_set_coefs_of_sections                                  &
     &       (pvr_scts_c%pvr_sect_ctl(i)%psf_def_c,                     &
     &        id_section_method, pvr_section_p%coefs(1:10,i), ierr)
          if(ierr .gt. 0) call calypso_mpi_abort                        &
     &         (ierr, 'Set section parameters for pvr')
!
          if(pvr_scts_c%pvr_sect_ctl(i)%opacity_ctl%iflag .gt. 0) then
            pvr_section_p%sect_opacity(i)                               &
     &        = pvr_scts_c%pvr_sect_ctl(i)%opacity_ctl%realvalue
          end if
        end do
      end if
!
      end subroutine set_control_pvr_sections
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_sections(pvr_section_p)
!
      type(pvr_section_parameter), intent(inout) :: pvr_section_p
!
!
      deallocate(pvr_section_p%coefs, pvr_section_p%sect_opacity)
!
      end subroutine dealloc_pvr_sections
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_sections(pvr_section_p)
!
      type(pvr_section_parameter), intent(inout) :: pvr_section_p
!
!
      allocate(pvr_section_p%coefs(10,pvr_section_p%num_sections))
      allocate(pvr_section_p%sect_opacity(pvr_section_p%num_sections))
!
      if(pvr_section_p%num_sections .gt. 0) then
        pvr_section_p%coefs =        zero
        pvr_section_p%sect_opacity = zero
      end if
!
      end subroutine alloc_pvr_sections
!
! -----------------------------------------------------------------------
!
      end module t_control_param_pvr_section
