!
!     module interpolate_on_node
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_scalar_node(np_smp, numnod, numele,     &
!     &          nnod_4_ele, ie, v_org, istack_smp, num_points,         &
!     &          iele_gauss, inod_gauss, vect)
!      subroutine s_interpolate_vector_node(np_smp, numnod, numele,     &!     &          nnod_4_ele, ie, v_org, istack_smp, num_points,         &
!     &          iele_gauss, inod_gauss, vect)
!      subroutine s_interpolate_tensor_node(np_smp, numnod, numele,     &
!     &          nnod_4_ele, ie, v_org, istack_smp, num_points,         &
!     &          iele_gauss, inod_gauss, vect)
!      subroutine s_interpolate_fields_node(np_smp, numnod, numele,     &
!     &          nnod_4_ele, ie, numdir, v_org, istack_smp, num_points, &
!     &          iele_gauss, inod_gauss, vect)
!
!      subroutine s_interpolate_imark_node(np_smp, numnod, numele,      &
!     &          nnod_4_ele, ie, imark_org, istack_smp, num_points,     &
!     &          iele_gauss, inod_gauss, imark)
!
      module interpolate_on_node
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_scalar_node(np_smp, numnod, numele,      &
     &          nnod_4_ele, ie, v_org, istack_smp, num_points,          &
     &          iele_gauss, inod_gauss, vect)
!
      use m_parallel_var_dof
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: inod_gauss(num_points)
      real (kind=kreal), intent(in) :: v_org(numnod)
!
      real (kind=kreal), intent(inout) :: vect(num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, k1
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,k1,i1)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
          iele =  iele_gauss(ig)
          k1 = inod_gauss(ig)
!
          i1 = ie(iele,k1)
!
          vect(ig  ) =  v_org(i1)
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_scalar_node
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_vector_node(np_smp, numnod, numele,      &
     &          nnod_4_ele, ie, v_org, istack_smp, num_points,          &
     &          iele_gauss, inod_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: inod_gauss(num_points)
      real (kind=kreal), intent(in) :: v_org(3*numnod)
!
      real (kind=kreal), intent(inout) :: vect(3*num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, k1
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,k1,i1)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
          iele =  iele_gauss(ig)
          k1 = inod_gauss(ig)
!
          i1 = ie(iele,k1)
!
          vect(3*ig-2) =  v_org(3*i1-2)
          vect(3*ig-1) =  v_org(3*i1-1)
          vect(3*ig  ) =  v_org(3*i1  )
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_vector_node
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_tensor_node(np_smp, numnod, numele,      &
     &          nnod_4_ele, ie, v_org, istack_smp, num_points,          &
     &          iele_gauss, inod_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: inod_gauss(num_points)
      real (kind=kreal), intent(in) :: v_org(6*numnod)
!
      real (kind=kreal), intent(inout) :: vect(6*num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, k1
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,k1,i1)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
          iele =  iele_gauss(ig)
          k1 = inod_gauss(ig)
!
          i1 = ie(iele,k1)
!
          vect(6*ig-5) =  v_org(6*i1-5)
          vect(6*ig-4) =  v_org(6*i1-4)
          vect(6*ig-3) =  v_org(6*i1-3)
          vect(6*ig-2) =  v_org(6*i1-2)
          vect(6*ig-1) =  v_org(6*i1-1)
          vect(6*ig  ) =  v_org(6*i1  )
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_tensor_node
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_fields_node(np_smp, numnod, numele,      &
     &          nnod_4_ele, ie, numdir, v_org, istack_smp, num_points,  &
     &          iele_gauss, inod_gauss, vect)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points, numdir
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: inod_gauss(num_points)
      real (kind=kreal), intent(in) :: v_org(numdir*numnod)
!
      real (kind=kreal), intent(inout) :: vect(numdir*num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, k1
      integer (kind = kint) :: ig, nd
!
!
!$omp parallel do private(ist,ied,ig,nd,iele,k1,i1)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do nd = 1, numdir
          do ig = ist, ied
            iele =  iele_gauss(ig)
            k1 = inod_gauss(ig)
!
            i1 = ie(iele,k1)
!
            vect(numdir*(ig-1)+nd) =  v_org(numdir*(i1-1)+nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_fields_node
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_imark_node(np_smp, numnod, numele,       &
     &          nnod_4_ele, ie, imark_org, istack_smp, num_points,      &
     &          iele_gauss, inod_gauss, imark)
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_smp(0:np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: inod_gauss(num_points)
      integer (kind = kint), intent(in) :: imark_org(numnod)
!
      integer (kind = kint), intent(inout) :: imark(num_points)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: iele, i1, k1
      integer (kind = kint) :: ig
!
!
!$omp parallel do private(ist,ied,ig,iele,k1,i1)
      do ip = 1, np_smp
        ist = istack_smp(ip-1) + 1
        ied = istack_smp(ip)
        do ig = ist, ied
          iele =  iele_gauss(ig)
          k1 = inod_gauss(ig)
          i1 = ie(iele,k1)
!
          imark(ig  ) =  imark_org(i1)
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_imark_node
!
! ----------------------------------------------------------------------
!
      end module interpolate_on_node
