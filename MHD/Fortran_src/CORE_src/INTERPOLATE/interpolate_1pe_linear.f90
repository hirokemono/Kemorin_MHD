!
!     module interpolate_1pe_linear
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_scalar_8(np_smp, numnod, numele,        &
!     &          ie, v_org, istack_wtype_smp, num_points,               &
!     &          iele_gauss, itype_gauss, xi_gauss, vect)
!      subroutine s_interpolate_vector_8(np_smp, numnod, numele,        &
!     &          ie, v_org, istack_wtype_smp, num_points,               &
!     &          iele_gauss, itype_gauss, xi_gauss, vect)
!      subroutine s_interpolate_tensor_8(np_smp, numnod, numele,        &
!     &          ie, v_org, istack_wtype_smp, num_points,               &
!     &          iele_gauss, itype_gauss, xi_gauss, vect)
!      subroutine s_interpolate_fields_8(np_smp, numnod, numele,        &
!     &          ie, numdir, v_org, istack_wtype_smp, num_points,       &
!     &          iele_gauss, itype_gauss, xi_gauss, vect)
!
      module interpolate_1pe_linear
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: nnod_4_ele = 8
      private :: nnod_4_ele
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_scalar_8(np_smp, numnod, numele,         &
     &          ie, v_org, istack_wtype_smp, num_points,                &
     &          iele_gauss, itype_gauss, xi_gauss, vect)
!
      use interpolate_on_node
      use interpolate_scalar_edge2
      use interpolate_scalar_surf4
      use interpolate_scalar_ele8
      use m_parallel_var_dof
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: vect(num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call s_interpolate_scalar_node(np_smp, numnod, numele,            &
     &    nnod_4_ele, ie, v_org, istack_wtype_smp(ist), num_points,     &
     &    iele_gauss, itype_gauss, vect)
!
      ist = np_smp
      call s_interpolate_scalar_edge2(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 2*np_smp
      call s_interpolate_scalar_surf4(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 3*np_smp
      call s_interpolate_scalar_ele8(np_smp, numnod, numele, ie,        &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    xi_gauss, vect)
!
      end subroutine s_interpolate_scalar_8
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_vector_8(np_smp, numnod, numele,         &
     &          ie, v_org, istack_wtype_smp, num_points,                &
     &          iele_gauss, itype_gauss, xi_gauss, vect)
!
      use interpolate_on_node
      use interpolate_vector_edge2
      use interpolate_vector_surf4
      use interpolate_vector_ele8
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(3*numnod)
!
      real (kind=kreal), intent(inout) :: vect(3*num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call s_interpolate_vector_node(np_smp, numnod, numele,            &
     &    nnod_4_ele, ie, v_org, istack_wtype_smp(ist), num_points,     &
     &    iele_gauss, itype_gauss, vect)
!
      ist = np_smp
      call s_interpolate_vector_edge2(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 2*np_smp
      call s_interpolate_vector_surf4(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 3*np_smp
      call s_interpolate_vector_ele8(np_smp, numnod, numele, ie,        &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    xi_gauss, vect)
!
      end subroutine s_interpolate_vector_8
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_tensor_8(np_smp, numnod, numele,         &
     &          ie, v_org, istack_wtype_smp, num_points,                &
     &          iele_gauss, itype_gauss, xi_gauss, vect)
!
      use interpolate_on_node
      use interpolate_tensor_edge2
      use interpolate_tensor_surf4
      use interpolate_tensor_ele8
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(6*numnod)
!
      real (kind=kreal), intent(inout) :: vect(6*num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call s_interpolate_tensor_node(np_smp, numnod, numele,            &
     &    nnod_4_ele, ie, v_org, istack_wtype_smp(ist), num_points,     &
     &    iele_gauss, itype_gauss, vect)
!
      ist = np_smp
      call s_interpolate_tensor_edge2(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 2*np_smp
      call s_interpolate_tensor_surf4(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 3*np_smp
      call s_interpolate_tensor_ele8(np_smp, numnod, numele, ie,        &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    xi_gauss, vect)
!
      end subroutine s_interpolate_tensor_8
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_fields_8(np_smp, numnod, numele,         &
     &          ie, numdir, v_org, istack_wtype_smp, num_points,        &
     &          iele_gauss, itype_gauss, xi_gauss, vect)
!
      use interpolate_on_node
      use interpolate_fields_edge2
      use interpolate_fields_surf4
      use interpolate_fields_ele8
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numdir*numnod)
!
      real (kind=kreal), intent(inout) :: vect(numdir*num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call s_interpolate_fields_node(np_smp, numnod, numele,            &
     &    nnod_4_ele, ie, numdir, v_org, istack_wtype_smp(ist),         &
     &    num_points, iele_gauss, itype_gauss, vect)
!
      ist = np_smp
      call s_interpolate_fields_edge2(np_smp, numnod, numele, ie,       &
     &    numdir, v_org, istack_wtype_smp(ist), num_points, iele_gauss, &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 2*np_smp
      call s_interpolate_fields_surf4(np_smp, numnod, numele, ie,       &
     &    numdir, v_org, istack_wtype_smp(ist), num_points, iele_gauss, &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 3*np_smp
      call s_interpolate_fields_ele8(np_smp, numnod, numele, ie,        &
     &    numdir, v_org, istack_wtype_smp(ist), num_points, iele_gauss, &
     &    xi_gauss, vect)
!
      end subroutine s_interpolate_fields_8
!
! ----------------------------------------------------------------------
!
      end module interpolate_1pe_linear
