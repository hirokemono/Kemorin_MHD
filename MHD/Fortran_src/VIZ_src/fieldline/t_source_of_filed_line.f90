!>@file   t_source_of_filed_line.f90
!!@brief  module t_source_of_filed_line
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Structure of start point data for line tracing iteration
!!
!!@verbatim
!!      subroutine alloc_local_start_grp_item(fln_src)
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!      subroutine alloc_start_point_fline(num_pe, fln_prm, fln_src)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!
!!      subroutine dealloc_local_start_grp_item(fln_src)
!!      subroutine dealloc_start_point_fline(fln_src)
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!@endverbatim
!
      module t_source_of_filed_line
!
      use m_precision
      use m_constants
      use t_control_params_4_fline
!
      implicit  none
!
!
      type each_fieldline_source
        integer(kind = kint) :: nele_start_grp = 0
        integer(kind = kint), allocatable :: iele_start_item(:,:)
        real(kind = kreal),   allocatable :: flux_start(:)
!
        integer(kind = kint) :: num_line_local = 0
        real(kind = kreal), allocatable :: xx4_initial_fline(:,:)
        real(kind = kreal), allocatable :: flux_start_fline(:)
!
        real(kind = kreal),   allocatable :: flux_stack_fline(:)
!>        outward flux flag
        integer(kind = kint), allocatable                               &
     &                       :: iflag_outward_flux_fline(:)
      end type each_fieldline_source
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_item(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      allocate(fln_src%iele_start_item(2,fln_src%nele_start_grp))
      allocate(fln_src%flux_start(fln_src%nele_start_grp))
      if(fln_src%nele_start_grp .gt. 0) fln_src%iele_start_item = 0
!
      end subroutine alloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_start_point_fline(num_pe, fln_prm, fln_src)
!
      integer, intent(in) :: num_pe
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: num
!
!
      allocate(fln_src%flux_stack_fline(0:num_pe))
      fln_src%flux_stack_fline = 0.0d0
!
      num = fln_prm%num_each_field_line
      allocate(fln_src%iflag_outward_flux_fline(num))
      allocate(fln_src%xx4_initial_fline(4,num))
      allocate(fln_src%flux_start_fline(num))
!
      fln_src%iflag_outward_flux_fline(1:num) =   0
      fln_src%xx4_initial_fline = 0.0d0
      fln_src%flux_start_fline =  0.0d0
!
      end subroutine alloc_start_point_fline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_local_start_grp_item(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
!
      deallocate(fln_src%iele_start_item, fln_src%flux_start)
!
      end subroutine dealloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_start_point_fline(fln_src)
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
      deallocate(fln_src%xx4_initial_fline, fln_src%flux_start_fline)
      deallocate(fln_src%flux_stack_fline)
      deallocate(fln_src%iflag_outward_flux_fline)
!
      end subroutine dealloc_start_point_fline
!
!  ---------------------------------------------------------------------
!
      end module t_source_of_filed_line
