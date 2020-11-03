!>@file   set_istack_4_domain_block.f90
!!@brief  module set_istack_4_domain_block
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Construct block stack list for re-partitioning
!!
!!@verbatim
!!      integer(kind = kint) function count_z_subdomain_num             &
!!     &                   (part_param, ndomain_z, istack_z_grp)
!!      integer(kind = kint) function count_yz_subdomain_num            &
!!     &         (part_param, num_nod_grp_z, idomain_nod_grp_z,         &
!!     &          ndomain_yz, istack_yz_grp)
!!        type(mesh_test_files_param), intent(in) :: part_param
!!
!!      subroutine set_z_subdomain_list                                 &
!!     &         (part_param, ndomain_z, istack_z_grp, num_nod_grp_z,   &
!!     &          idomain_nod_grp_z)
!!      subroutine set_yz_subdomain_list                                &
!!     &         (part_param, num_nod_grp_z, idomain_nod_grp_z,         &
!!     &          ndomain_yz, istack_yz_grp, num_nod_grp_yz,            &
!!     &          idomain_nod_grp_yz)
!!        type(mesh_test_files_param), intent(in) :: part_param
!!
!!      subroutine set_istack_xyz_domain_block                          &
!!     &         (node, inod_sort, id_block, volume_nod,                &
!!     &          nblock_x, sub_volume, ndomain_x, ndomain_yz,          &
!!     &          istack_yz_grp, istack_vol, vol_grp)
!!        type(node_data), intent(in) :: node
!!@endverbatim
!
      module set_istack_4_domain_block
!
      use m_precision
      use t_control_param_vol_grping
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function count_z_subdomain_num               &
     &                   (part_param, ndomain_z, istack_z_grp)
!
      type(mesh_test_files_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: ndomain_z
      integer(kind = kint), intent(in) :: istack_z_grp(0:ndomain_z)
!
      integer(kind = kint) :: icou, iz, num
!
      icou = 0
      do iz = 1, part_param%ndomain_eb(3)
        num = istack_z_grp(iz) - istack_z_grp(iz-1)
        if(num .gt. 0) icou = icou + 1
      end do
      count_z_subdomain_num = icou
!
      end function count_z_subdomain_num
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function count_yz_subdomain_num              &
     &         (part_param, num_nod_grp_z, idomain_nod_grp_z,           &
     &          ndomain_yz, istack_yz_grp)
!
      type(mesh_test_files_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_z(num_nod_grp_z+1)
      integer(kind = kint), intent(in) :: ndomain_yz
      integer(kind = kint), intent(in) :: istack_yz_grp(0:ndomain_yz)
!
      integer(kind = kint) :: i, icou, iy, iz, jk, num
!
!
      icou = 0
      do i = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(i)
        do iy = 1, part_param%ndomain_eb(2)
          jk = iy + (iz-1) * part_param%ndomain_eb(2)
          num = istack_yz_grp(jk) - istack_yz_grp(jk-1)
          if(num .gt. 0) icou = icou + 1
        end do
      end do
      count_yz_subdomain_num = icou
!
      end function count_yz_subdomain_num
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_z_subdomain_list                                   &
     &         (part_param, ndomain_z, istack_z_grp, num_nod_grp_z,     &
     &          idomain_nod_grp_z)
!
      type(mesh_test_files_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: ndomain_z
      integer(kind = kint), intent(in) :: istack_z_grp(0:ndomain_z)
!
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(inout)                               &
     &     :: idomain_nod_grp_z(num_nod_grp_z+1)
!
      integer(kind = kint) :: icou, iz, num
!
!
!$omp parallel do private(icou)
      do icou = 1, num_nod_grp_z+1
        idomain_nod_grp_z(icou) = 0
      end do
!$omp end parallel do
!
      icou = 0
      do iz = 1, part_param%ndomain_eb(3)
        num = istack_z_grp(iz) - istack_z_grp(iz-1)
        if(num .gt. 0) then
          icou = icou + 1
          idomain_nod_grp_z(icou) = iz
        end if
      end do
      idomain_nod_grp_z(num_nod_grp_z+1) = part_param%ndomain_eb(3) + 1
!
      end subroutine set_z_subdomain_list
!
! ----------------------------------------------------------------------
!
      subroutine set_yz_subdomain_list                                  &
     &         (part_param, num_nod_grp_z, idomain_nod_grp_z,           &
     &          ndomain_yz, istack_yz_grp, num_nod_grp_yz,              &
     &          idomain_nod_grp_yz)
!
      type(mesh_test_files_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_z(num_nod_grp_z+1)
      integer(kind = kint), intent(in) :: ndomain_yz
      integer(kind = kint), intent(in) :: istack_yz_grp(0:ndomain_yz)
!
      integer(kind = kint), intent(in) :: num_nod_grp_yz
      integer(kind = kint), intent(inout)                               &
     &     :: idomain_nod_grp_yz(num_nod_grp_yz+1)
!
      integer(kind = kint) :: i, icou, iy, iz, jk, num
!
!
!$omp parallel do private(icou)
      do icou = 1, num_nod_grp_yz+1
        idomain_nod_grp_yz(icou) = 0
      end do
!$omp end parallel do
!
      icou = 0
      do i = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(i)
        do iy = 1, part_param%ndomain_eb(2)
          jk = iy + (iz-1) * part_param%ndomain_eb(2)
          num = istack_yz_grp(jk) - istack_yz_grp(jk-1)
          if(num .gt. 0) then
            icou = icou + 1
            idomain_nod_grp_yz(icou)                                    &
     &           = iy + (iz-1) * part_param%ndomain_eb(2)
!            idomain_nod_grp_yz(2,icou) = iy
!            idomain_nod_grp_yz(3,icou) = iz
          end if
        end do
      end do
      idomain_nod_grp_yz(num_nod_grp_yz+1)                              &
     &          = part_param%ndomain_eb(2)*part_param%ndomain_eb(3) + 1
!      idomain_nod_grp_yz(2,num_nod_grp_yz+1)                           &
!     &          = part_param%ndomain_eb(2)+1
!      idomain_nod_grp_yz(3,num_nod_grp_yz+1) = part_param%ndomain_eb(3)
!
      end subroutine set_yz_subdomain_list
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_istack_xyz_domain_block                            &
     &         (node, inod_sort, id_block, volume_nod,                  &
     &          nblock_x, sub_volume, ndomain_x, ndomain_yz,            &
     &          istack_yz_grp, istack_vol, vol_grp)
!
      use t_geometry_data
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_real
      use transfer_to_long_integers
!
      implicit none
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
      integer(kind = kint), intent(in) :: id_block(node%numnod)
      real(kind = kreal), intent(in) :: volume_nod(node%numnod)
!
      integer(kind = kint), intent(in) :: nblock_x
      integer(kind = kint), intent(in) :: ndomain_x, ndomain_yz
      integer(kind = kint), intent(in) :: istack_yz_grp(0:ndomain_yz)
      real(kind = kreal), intent(in) :: sub_volume
!
      integer(kind = kint), intent(inout)                               &
     &                      :: istack_vol(0:ndomain_x,ndomain_yz)
      real(kind = kreal), intent(inout)                                 &
     &                      :: vol_grp(ndomain_x,ndomain_yz)
!
!      integer(kind = kint) :: ip
      integer(kind = kint) :: jk, ist, ied, i, j, inod, inum
      real(kind = kreal), allocatable :: vol_block_lc(:)
      real(kind = kreal), allocatable :: vol_block_gl(:)
      real(kind = kreal) :: vol_ref
!
!
      allocate(vol_block_lc(nblock_x))
      allocate(vol_block_gl(nblock_x))
!
      do jk = 1, ndomain_yz
        vol_block_lc(1:nblock_x) = 0.0d0
        ist = istack_yz_grp(jk-1) + 1
        ied = istack_yz_grp(jk  )
        do inum = ist, ied
          inod = inod_sort(inum)
          i = id_block(inod)
          vol_block_lc(i) = vol_block_lc(i) + volume_nod(inod)
        end do
!
        call calypso_mpi_reduce_real(vol_block_lc, vol_block_gl,        &
     &      cast_long(nblock_x), MPI_SUM, int(jk-1))
      end do
      deallocate(vol_block_lc)
!
!      do ip = 1, ndomain_yz
!      if(ip-1 .eq. my_rank) then
      if(my_rank .lt. ndomain_yz) then
        jk = 1 + my_rank
!
        vol_ref = 0.0d0
        vol_grp(1:ndomain_x,jk) = 0.0d0
        istack_vol(0,jk) = 0
        do i = 1, nblock_x
          vol_ref = vol_ref + vol_block_gl(i)
          j = min(1+int(vol_ref / sub_volume),ndomain_x)
          istack_vol(j,jk) = i
          vol_grp(j,jk) = vol_ref
        end do
!
        do j = ndomain_x, 2, - 1
          vol_grp(j,jk) = vol_grp(j,jk) - vol_grp(j-1,jk)
        end do
      end if
!      end if
!      end do
      deallocate(vol_block_gl)
!
      call calypso_mpi_barrier
      do jk = 1, ndomain_yz
        call calypso_mpi_bcast_int(istack_vol(0,jk),                    &
     &                             cast_long(ndomain_x+1), int(jk-1))
        call calypso_mpi_bcast_real(vol_grp(1,jk),                      &
     &                              cast_long(ndomain_x), int(jk-1))
      end do
!
      end subroutine set_istack_xyz_domain_block
!
! ----------------------------------------------------------------------
!
      end module set_istack_4_domain_block
