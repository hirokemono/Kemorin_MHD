!sort_cube_4_rcb.f90
!     module sort_cube_4_rcb
!
!     written by H. Matsui on Aug., 2007
!
!      subroutine s_sort_cube_4_rcb(numnod, num_cube, NP,               &
!     &          iradius, itheta, iphi, xx, longitude, inod_free,       &
!     &          num_local, IGROUP, IS1, VAL)
!
      module sort_cube_4_rcb
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind = kreal), allocatable :: phi_cube(:)
      real(kind = kreal), allocatable :: neg_z_cube(:)
!
      private :: phi_cube, neg_z_cube
      private :: allocate_work_4_cube_rcb, deallocate_work_4_cube_rcb
      private :: copy_cube_position_zonal_rcb
      private :: copy_cube_position_vert_rcb
      private :: set_cube_domain_list_by_phi
      private :: set_cube_domain_list_by_z
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_sort_cube_4_rcb(numnod, num_cube, NP,                &
     &          iradius, itheta, iphi, xx, longitude, inod_free,        &
     &          num_local, IGROUP, IS1, VAL)
!
      use sort_by_position_4_rcb
      use quicksort
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: longitude(numnod)
!
      integer(kind = kint), intent(in) :: num_cube
      integer(kind = kint), intent(in) :: inod_free(num_cube)
!
      integer(kind = kint), intent(in) :: NP, iradius, itheta, iphi
      integer(kind = kint), intent(in) :: num_local(NP)
!
      integer(kind = kint), intent(inout) :: IGROUP(numnod)
      integer(kind = kint), intent(inout) :: IS1(numnod)
      real(kind = kreal), intent(inout) :: VAL(numnod)
!
      integer(kind = kint) :: ip, ncou
!
!
      call allocate_work_4_cube_rcb(num_cube)
!
      call copy_cube_position_4_rcb(numnod, num_cube, inod_free,        &
     &    xx(1,3), longitude, phi_cube, neg_z_cube)
!
!  devide around center
!
      call copy_cube_position_zonal_rcb(numnod, num_cube, inod_free,    &
     &    phi_cube, VAL, IS1)
!
      call quicksort_real_w_index(numnod, VAL, ione, num_cube, IS1)
!
      call sorting_by_2nd_direction(numnod, num_cube, phi_cube,         &
     &    neg_z_cube, VAL, IS1)
!
      call set_cube_domain_list_by_phi(NP, numnod, IGROUP,              &
     &    iradius, itheta, iphi, num_local, IS1)
!
!  theta_direction
!
      do ip = 1, iphi*iradius
!
        call copy_cube_position_vert_rcb(numnod, num_cube, ip, ncou,    &
     &      IGROUP, inod_free, neg_z_cube, VAL, IS1)
!
        call quicksort_real_w_index(numnod, VAL(1), ione, ncou, IS1)
!
        call sorting_by_2nd_direction(numnod, ncou, neg_z_cube,         &
     &      phi_cube, VAL, IS1)
!
        call set_cube_domain_list_by_z(NP, numnod, ip, IGROUP,          &
     &      iradius, itheta, iphi,  num_local, IS1)
!
      end do
!
      call deallocate_work_4_cube_rcb
!
      end subroutine s_sort_cube_4_rcb
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine allocate_work_4_cube_rcb(num_cube)
!
      integer(kind = kint), intent(in) :: num_cube
!
      allocate(neg_z_cube(num_cube))
      allocate(phi_cube(num_cube))
!
      neg_z_cube = 0.0d0
      phi_cube = 0.0d0
!
      end subroutine allocate_work_4_cube_rcb
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_work_4_cube_rcb
!
      deallocate(neg_z_cube)
      deallocate(phi_cube)
!
      end subroutine deallocate_work_4_cube_rcb
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_cube_position_4_rcb(nnod, num_cube, inod_free,    &
     &          zz, phi, phi_cube, neg_z_cube)
!
      integer(kind = kint), intent(in) :: nnod, num_cube
      integer(kind = kint), intent(in) :: inod_free(num_cube)
      real(kind = kreal), intent(in) :: phi(nnod)
      real(kind = kreal), intent(in) :: zz(nnod)
!
      real(kind = kreal), intent(inout) :: phi_cube(num_cube)
      real(kind = kreal), intent(inout) :: neg_z_cube(num_cube)
!
      integer(kind = kint) :: i, inod
!
      do i = 1, num_cube
        inod = inod_free(i)
!        theta_cube(i) = colatitude(inod)
        phi_cube(i) =     phi(inod)
        neg_z_cube(i) = - zz(inod)
      end do
!
      end subroutine copy_cube_position_4_rcb
!
!   --------------------------------------------------------------------
!
      subroutine copy_cube_position_zonal_rcb(nnod, num_cube,           &
     &          inod_free, phi_cube, VAL, IS1)
!
      integer(kind = kint), intent(in) :: nnod, num_cube
      integer(kind = kint), intent(in) :: inod_free(num_cube)
      real(kind = kreal), intent(in) :: phi_cube(num_cube)
!
      real(kind = kreal), intent(inout) :: VAL(nnod)
      integer(kind = kint), intent(inout) :: IS1(nnod)
!
      integer(kind = kint) :: i
!
      do i = 1, num_cube
        IS1(i)= inod_free(i)
        VAL(i)= phi_cube(i)
      end do
!
      end subroutine copy_cube_position_zonal_rcb
!
!   --------------------------------------------------------------------
!
      subroutine copy_cube_position_vert_rcb(nnod, num_cube, ip, ncou,  &
     &          IGROUP, inod_free, neg_z_cube, VAL, IS1)
!
      integer(kind = kint), intent(in) :: nnod, num_cube, ip
      integer(kind = kint), intent(in) :: IGROUP(nnod)
      integer(kind = kint), intent(in) :: inod_free(num_cube)
      real(kind = kreal), intent(in) :: neg_z_cube(num_cube)
!
      integer(kind = kint), intent(inout) :: ncou
      integer(kind = kint), intent(inout) :: IS1(nnod)
      real(kind = kreal), intent(inout) :: VAL(nnod)
!
      integer(kind = kint) :: i, inod
!
      ncou= 0
      do i = 1, num_cube
        inod = inod_free(i)
        if (IGROUP(inod) .eq. ip) then
          ncou = ncou + 1
          IS1(ncou)= inod
!          VAL(ncou)=  colat_cube(i)
          VAL(ncou)= neg_z_cube(i)
        end if
      end do
!
      end subroutine copy_cube_position_vert_rcb
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_cube_domain_list_by_phi(NP, nnod, IGROUP,          &
     &          iradius, itheta, iphi, num_local, IS1)
!
      integer(kind = kint), intent(in) :: NP, nnod
      integer(kind = kint), intent(in) :: iradius, itheta, iphi
      integer(kind = kint), intent(in) :: num_local(NP)
      integer(kind = kint), intent(in) :: IS1(nnod)
!
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: icou, j, inod, ip, ip0, inum, num
!
!
      icou = 0
      do ip = 1, iphi*iradius
        num = 0
        do j = 1, itheta
          ip0 = ip + (j-1)*iphi*iradius
          num = num + num_local(ip0)
        end do
        do inum = 1, num
          icou = icou + 1
          inod = IS1(icou)
          IGROUP(inod) = ip
        end do
      end do
!
      end subroutine set_cube_domain_list_by_phi
!
!   --------------------------------------------------------------------
!
      subroutine set_cube_domain_list_by_z(NP, nnod, ip, IGROUP,        &
     &          iradius, itheta, iphi, num_local, IS1)
!
      integer(kind = kint), intent(in) :: NP, nnod
      integer(kind = kint), intent(in) :: iradius, itheta, iphi
      integer(kind = kint), intent(in) :: num_local(NP)
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint), intent(in) :: Is1(nnod)
!
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
!
      integer(kind = kint) :: icou, j, inod, inum, ip0
!
!
      icou = 0
      do j = 1, itheta
        ip0 = ip + (j-1)*iphi*iradius
        do inum = 1, num_local(ip0)
          icou = icou + 1
          inod = IS1(icou)
          IGROUP(inod) = IGROUP(inod) + (j-1)*iphi*iradius
        end do
      end do
!
      end subroutine set_cube_domain_list_by_z
!
!   --------------------------------------------------------------------
!
      end module sort_cube_4_rcb
      