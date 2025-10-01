!>@file   t_ctl_param_masking.f90
!!@brief  module t_ctl_param_masking
!!
!!@author Y. Lyao and H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief control parameters for parallel LIC
!!
!!@verbatim
!!      logical function multi_mask_flag(num_masking, masking, value)
!!        integer(kind=kint), intent(in) :: num_masking
!!        type(masking_parameter), intent(in) :: masking(num_masking)
!!        real(kind=kreal), intent(in) :: value(num_masking)
!!      logical function single_mask_flag(masking, value)
!!        type(masking_parameter), intent(in) :: masking
!!        real(kind=kreal), intent(in) :: value
!!
!!      subroutine dealloc_masking_range(masking)
!!      subroutine alloc_masking_range(num, masking)
!!        type(masking_parameter), intent(inout) :: masking
!!@endverbatim
!
      module t_ctl_param_masking
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use skip_comment_f
!
      implicit  none
!
      character(len=kchara), parameter                                  &
     &                      :: hd_masking_geometry = 'geometry'
      character(len=kchara), parameter                                  &
     &                     :: hd_masking_field = 'field'
!
!>        Mask type 1: geometry
      integer(kind = kint), parameter :: iflag_geometrymask = 1
!>        Mask type 2: field data
      integer(kind = kint), parameter :: iflag_fieldmask =    2
!

!>      Structure of masking parameter
      type masking_parameter
!>        Mask type 1: geometry 2: field data
        integer(kind = kint) :: mask_type =   iflag_geometrymask
!
!>     Field type for masking data
        integer(kind = kint) :: id_mask_field = 0
!>     Component flag for masking data
        integer(kind = kint) :: id_mask_comp =  0
!
!>        Number of masking range
        integer(kind = kint) :: num_range = 1
!>        minimum value of source point range
        real(kind = kreal), allocatable :: range_min(:)
!>        maximum value of source point range
        real(kind = kreal), allocatable :: range_max(:)
      end type masking_parameter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      logical function multi_mask_flag(num_masking, masking, value)
!
      integer(kind=kint), intent(in) :: num_masking
      type(masking_parameter), intent(in) :: masking(num_masking)
      real(kind=kreal), intent(in) :: value(num_masking)
!
      integer(kind=kint) :: i
!
      multi_mask_flag = .TRUE.
      do i = 1, num_masking
        multi_mask_flag = single_mask_flag(masking(i), value(i))
        if(multi_mask_flag .eqv. .FALSE.) return
      end do
!
      end function multi_mask_flag
!
!  ---------------------------------------------------------------------
!
      logical function single_mask_flag(masking, value)
!
      type(masking_parameter), intent(in) :: masking
      real(kind=kreal), intent(in) :: value
!
      integer(kind=kint) :: j
!
      single_mask_flag = .TRUE.
      do j = 1, masking%num_range
        if(      (value .lt. masking%range_min(j))                      &
     &      .or. (value .gt. masking%range_max(j))) then
          single_mask_flag = .FALSE.
          return
        end if
      end do
!
      end function single_mask_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_masking_range(masking)
!
      type(masking_parameter), intent(inout) :: masking
!
      deallocate(masking%range_min, masking%range_max)
!
      end subroutine dealloc_masking_range
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_masking_range(num, masking)
!
      integer(kind = kint), intent(in) :: num
      type(masking_parameter), intent(inout) :: masking
!
      masking%num_range = num
      allocate(masking%range_min(masking%num_range))
      allocate(masking%range_max(masking%num_range))
!
      if(masking%num_range .gt. 0) then
        masking%range_min(1:masking%num_range) = -1.0e15
        masking%range_max(1:masking%num_range) =  1.0e15
      end if
!
      end subroutine alloc_masking_range
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_param_masking
