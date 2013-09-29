!
!     module interpolate_para_linear
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interporate_scalar_para_8(np_smp, numnod, numele,   &
!     &          ie, v_org, num_dest_domain, istack_tbl_wtype_smp,      &
!     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!      subroutine s_interporate_vector_para_8(np_smp, numnod, numele,   &
!     &          ie, v_org, num_dest_domain, istack_tbl_wtype_smp,      &
!     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!      subroutine s_interporate_tensor_para_8(np_smp, numnod, numele,   &
!     &          ie, v_org, num_dest_domain, istack_tbl_wtype_smp,      &
!     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
!      subroutine s_interporate_fields_para_8(np_smp, numnod, numele,   &
!     &          ie, numdir, v_org, num_dest_domain,                    &
!     &          istack_tbl_wtype_smp, num_points, iele_gauss,          &
!     &          itype_gauss, xi_gauss, vect)
!
      module interpolate_para_linear
!
      use m_precision
!
      use m_geometry_constants
      use interpolate_1pe_linear
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_scalar_para_8(np_smp, numnod, numele,    &
     &          ie, v_org, num_dest_domain, istack_tbl_wtype_smp,       &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      use calypso_mpi
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: vect(num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
!
        ist = 4*np_smp*(ip-1)
        call s_interpolate_scalar_8(np_smp, numnod, numele,             &
     &      ie, v_org, istack_tbl_wtype_smp(ist), num_points,           &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
!
      end do
!
      end subroutine s_interporate_scalar_para_8
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_vector_para_8(np_smp, numnod, numele,    &
     &          ie, v_org, num_dest_domain, istack_tbl_wtype_smp,       &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(3*numnod)
!
      real (kind=kreal), intent(inout) :: vect(3*num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
!
        ist = 4*np_smp*(ip-1)
        call s_interpolate_vector_8(np_smp, numnod, numele,             &
     &      ie, v_org, istack_tbl_wtype_smp(ist), num_points,           &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
!
      end do
!
      end subroutine s_interporate_vector_para_8
!
!  ---------------------------------------------------------------------
!
      subroutine s_interporate_tensor_para_8(np_smp, numnod, numele,    &
     &          ie, v_org, num_dest_domain, istack_tbl_wtype_smp,       &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(6*numnod)
!
      real (kind=kreal), intent(inout) :: vect(6*num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
!
        ist = 4*np_smp*(ip-1)
        call s_interpolate_tensor_8(np_smp, numnod, numele,             &
     &      ie, v_org, istack_tbl_wtype_smp(ist), num_points,           &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
!
      end do
!
      end subroutine s_interporate_tensor_para_8
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_interporate_fields_para_8(np_smp, numnod, numele,    &
     &          ie, numdir, v_org, num_dest_domain,                     &
     &          istack_tbl_wtype_smp, num_points, iele_gauss,           &
     &          itype_gauss, xi_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, numdir
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(numdir*numnod)
!
      real (kind=kreal), intent(inout) :: vect(numdir*num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
!
        ist = 4*np_smp*(ip-1)
        call s_interpolate_fields_8(np_smp, numnod, numele,             &
     &      ie, numdir, v_org, istack_tbl_wtype_smp(ist), num_points,   &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
!
      end do
!
      end subroutine s_interporate_fields_para_8
!
! ----------------------------------------------------------------------
!
      end module interpolate_para_linear
