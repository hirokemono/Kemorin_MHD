!>@file  t_lic_field_data.f90
!!       module t_lic_field_data
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine alloc_nod_vector_4_lic                               &
!!     &         (numnod, num_masking, field_lic)
!!      subroutine dealloc_nod_data_4_lic(field_lic)
!!@endverbatim
!
      module t_lic_field_data
!
      use m_precision
!
      use m_constants
!
      implicit  none
!
!
!
!>  Structure for field data for LIC
      type lic_field_data
!>    Data for rendering
        real(kind = kreal), allocatable :: d_lic(:)
!
!>    Vector Data for LIC
        real(kind = kreal), allocatable :: v_lic(:,:)
!>    Vector Data for LIC opacity
!        real(kind = kreal), allocatable :: o_lic(:)
!>    Number of LIC masking data fiels
        integer(kind = kint) :: num_mask = 0
!>    Vector Data for LIC masking data
        real(kind = kreal), allocatable :: s_lic(:,:)
      end type lic_field_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_vector_4_lic                                 &
     &         (numnod, num_masking, field_lic)
!
      integer(kind = kint), intent(in) :: numnod, num_masking
      type(lic_field_data), intent(inout) :: field_lic
!
!
      allocate(field_lic%d_lic(numnod))
      allocate(field_lic%v_lic(numnod,3))
!      allocate(field_lic%o_lic(numnod))
      if(numnod .gt. 0) field_lic%d_lic =    0.0d0
      if(numnod .gt. 0) field_lic%v_lic =    0.0d0
!      if(numnod .gt. 0) field_lic%o_lic =    0.0d0
!
      field_lic%num_mask = num_masking
      allocate(field_lic%s_lic(numnod,field_lic%num_mask))
      if(numnod*num_masking .gt. 0) field_lic%s_lic = 0.0d0
!
      end subroutine alloc_nod_vector_4_lic
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_data_4_lic(field_lic)
!
      type(lic_field_data), intent(inout) :: field_lic
!
!
      deallocate(field_lic%v_lic)
!      deallocate(field_lic%o_lic)
!
      deallocate(field_lic%s_lic)
      deallocate(field_lic%d_lic)
!
      end subroutine dealloc_nod_data_4_lic
!
! -----------------------------------------------------------------------
!
      end module t_lic_field_data
