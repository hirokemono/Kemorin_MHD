!>@file  t_geometries_in_pvr_screen.f90
!!       module t_geometries_in_pvr_screen
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!@endverbatim
!
      module t_geometries_in_pvr_screen
!
      use m_precision
!
      use m_constants
!
      implicit  none
!
!
!>  Structure for field data on projected coordinate
      type pvr_projected_field
!>    Data for rendering
        real(kind = kreal), pointer :: d_pvr(:)
!>    Gradient for rendering
        real(kind = kreal), pointer :: grad_ele(:,:)
!
!>    flag for rendering element
        integer(kind = kint), pointer :: iflag_used_ele(:)
      end type pvr_projected_field
!
!>  Structure for data on projected coordinate
      type pvr_projected_type
!>    Number of node
        integer(kind = kint) :: nnod_pvr
!>    node stack for SMP
        integer(kind = kint), pointer :: istack_nod_pvr(:)
!>    Position in physical coordinate
        real(kind = kreal), pointer :: x_nod_sim(:,:)
!>    Position in modelview coordinate
        real(kind = kreal), pointer :: x_nod_model(:,:)
!>    Position in screen coordinate
        real(kind = kreal), pointer :: x_nod_screen(:,:)
!>    Number of element
        integer(kind = kint) :: nele_pvr
!
!>    Data for rendering
        type(pvr_projected_field), pointer :: field_pvr(:)
      end type pvr_projected_type
!
!>  Structure for pixel position
      type pvr_pixel_position_type
!>    Number of horizontal pixels
        integer(kind = kint) :: num_pixel_x
!>    Number of vertical pixels
        integer(kind = kint) :: num_pixel_y
!>    Position of horizontal pixels
        real(kind = kreal), allocatable :: pixel_point_x(:)
!>    Position of vertical pixels
        real(kind = kreal), allocatable :: pixel_point_y(:)
      end type pvr_pixel_position_type
!
      private :: alloc_nod_data_4_pvr, alloc_iflag_pvr_used_ele
      private :: dealloc_data_4_pvr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_data_4_pvr(nnod, nele, fld_pvr)
!
      integer(kind = kint), intent(in) :: nnod, nele
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      allocate(fld_pvr%d_pvr(nnod))
      allocate(fld_pvr%grad_ele(nele,3))
      if(nnod .gt. 0) fld_pvr%d_pvr =    0.0d0
      if(nele .gt. 0) fld_pvr%grad_ele = 0.0d0
!
      end subroutine alloc_nod_data_4_pvr
!
! -----------------------------------------------------------------------
!
      subroutine alloc_iflag_pvr_used_ele(nele, fld_pvr)
!
      integer(kind = kint), intent(in) :: nele
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      allocate(fld_pvr%iflag_used_ele(nele))
      if(nele .gt. 0) fld_pvr%iflag_used_ele = 0
!
      end subroutine alloc_iflag_pvr_used_ele
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_data_4_pvr(fld_pvr)
!
      type(pvr_projected_field), intent(inout) :: fld_pvr
!
!
      deallocate(fld_pvr%iflag_used_ele)
      deallocate(fld_pvr%d_pvr, fld_pvr%grad_ele)
!
      end subroutine dealloc_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_node_position_pvr(numnod, numele, proj)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(pvr_projected_type), intent(inout) :: proj
!
!
      proj%nnod_pvr = numnod
      proj%nele_pvr = numele
!
      allocate(proj%istack_nod_pvr(0:np_smp))
      allocate(proj%x_nod_sim(proj%nnod_pvr,4))
      allocate(proj%x_nod_model(proj%nnod_pvr,4))
      allocate(proj%x_nod_screen(proj%nnod_pvr,4))
!
      proj%istack_nod_pvr = 0
      if(proj%nnod_pvr .gt. 0) then
        proj%x_nod_sim =   0.0d0
        proj%x_nod_model =  0.0d0
        proj%x_nod_screen = 0.0d0
      end if
!
      end subroutine allocate_node_position_pvr
!
! -----------------------------------------------------------------------
!
      subroutine allocate_nod_data_4_pvr(num_pvr, proj)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_projected_type), intent(inout) :: proj
!
      integer(kind = kint) :: i
!
!
      allocate(proj%field_pvr(num_pvr))
      do i = 1, num_pvr
        call alloc_nod_data_4_pvr                                       &
     &     (proj%nnod_pvr, proj%nele_pvr, proj%field_pvr(i))
        call alloc_iflag_pvr_used_ele(proj%nele_pvr, proj%field_pvr(i))
      end do
!
      end subroutine allocate_nod_data_4_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_pixel_position_pvr(pixel_xy)
!
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      allocate(pixel_xy%pixel_point_x(pixel_xy%num_pixel_x))
      allocate(pixel_xy%pixel_point_y(pixel_xy%num_pixel_y))
      pixel_xy%pixel_point_x =  0.0d0
      pixel_xy%pixel_point_y =  0.0d0
!
      end subroutine allocate_pixel_position_pvr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_projected_data_pvr(num_pvr, proj)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_projected_type), intent(inout) :: proj
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_pvr
        call dealloc_data_4_pvr(proj%field_pvr(i))
      end do
!
      deallocate(proj%istack_nod_pvr)
      deallocate(proj%x_nod_sim, proj%x_nod_model, proj%x_nod_screen)
!
      end subroutine deallocate_projected_data_pvr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pixel_position_pvr(pixel_xy)
!
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
!
      deallocate(pixel_xy%pixel_point_x, pixel_xy%pixel_point_y)
!
      end subroutine deallocate_pixel_position_pvr
!
! -----------------------------------------------------------------------
!
      end module t_geometries_in_pvr_screen
