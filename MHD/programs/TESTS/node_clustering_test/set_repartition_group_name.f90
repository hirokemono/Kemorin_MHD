!>@file   set_repartition_group_name.f90
!!@brief  module set_repartition_group_name
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Set group names for re-partitiong groups
!!
!!@verbatim
!!      subroutine set_z_domain_grp_name(part_param, base_name,         &
!!     &          num_domain_grp, domain_grp_name)
!!      subroutine set_yz_domain_grp_name(part_param, base_name,        &
!!     &          num_domain_grp, domain_grp_name)
!!      subroutine set_xyz_domain_grp_name(part_param, base_name,       &
!!     &          num_domain_grp, domain_grp_name)
!!        type(volume_partioning_param), intent(in) :: part_param
!!
!!      subroutine check_stacks_4_z_domain(my_rank, node, part_param,   &
!!     &          inod_sort, num_nod_group_z, istack_nod_grp_z)
!!      subroutine check_stacks_4_yz_domain(my_rank, node, part_param,  &
!!     &          inod_sort, num_nod_grp_z, idomain_nod_grp_z,          &
!!     &          num_nod_group_yz, istack_nod_grp_y)
!!      subroutine check_stacks_4_new_domain(my_rank, node, part_param, &
!!     &          inod_sort, num_nod_grp_yz, idomain_nod_grp_yz,        &
!!     &          num_nod_grp_xyz, istack_nod_grp_xyz)
!!        type(node_data), intent(in) :: node
!!        type(volume_partioning_param), intent(in) :: part_param
!!@endverbatim
!
      module set_repartition_group_name
!
      use m_precision
      use m_constants
      use t_geometry_data
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
      subroutine set_z_domain_grp_name(part_param, base_name,           &
     &          num_domain_grp, domain_grp_name)
!
      use set_parallel_file_name
!
      type(volume_partioning_param), intent(in) :: part_param
      character(len = kchara), intent(in) :: base_name
      integer(kind = kint), intent(in) :: num_domain_grp
!
      character(len = kchara), intent(inout)                            &
     &                        :: domain_grp_name(num_domain_grp)
!
      character(len = kchara) :: chara_tmp
!
      integer(kind = kint) :: iz
!
!$omp parallel do private(iz)
      do iz = 1, part_param%ndomain_eb(3)
        write(chara_tmp,'(a,a2)') trim(base_name), '_z'
        call add_index_after_name(iz, chara_tmp, domain_grp_name(iz))
      end do
!$omp end parallel do
!
      end subroutine set_z_domain_grp_name
!
! ----------------------------------------------------------------------
!
      subroutine set_yz_domain_grp_name(part_param, base_name,          &
     &          num_domain_grp, domain_grp_name)
!
      use set_parallel_file_name
!
      type(volume_partioning_param), intent(in) :: part_param
      character(len = kchara), intent(in) :: base_name
      integer(kind = kint), intent(in) :: num_domain_grp
!
      character(len = kchara), intent(inout)                            &
     &                        :: domain_grp_name(num_domain_grp)
!
      character(len = kchara) :: chara_tmp
!
      integer(kind = kint) :: iz, iy, jk
!
!$omp parallel do private(iy,iz,jk)
      do iz = 1, part_param%ndomain_eb(3)
        do iy = 1, part_param%ndomain_eb(2)
          jk = iy + (iz-1) * part_param%ndomain_eb(2)
!
          write(chara_tmp,'(a,a2)') trim(base_name),'_y'
          call add_index_after_name(iy, chara_tmp, domain_grp_name(jk))
          write(chara_tmp,'(a,a2)') trim(domain_grp_name(jk)),'_z'
          call add_index_after_name(iz, chara_tmp, domain_grp_name(jk))
        end do
      end do
!$omp end parallel do
!
      end subroutine set_yz_domain_grp_name
!
! ----------------------------------------------------------------------
!
      subroutine set_xyz_domain_grp_name(part_param, base_name,         &
     &          num_domain_grp, domain_grp_name)
!
      use set_parallel_file_name
!
      type(volume_partioning_param), intent(in) :: part_param
      character(len = kchara), intent(in) :: base_name
      integer(kind = kint), intent(in) :: num_domain_grp
!
      character(len = kchara), intent(inout)                            &
     &                        :: domain_grp_name(num_domain_grp)
!
      character(len = kchara) :: chara_tmp
!
      integer(kind = kint) :: iz, iy, ix, jk, i
!
!$omp parallel do private(i,ix,iy,iz,jk)
      do iz = 1, part_param%ndomain_eb(3)
        do iy = 1, part_param%ndomain_eb(2)
          jk = iy + (iz-1) * part_param%ndomain_eb(2)
          do ix = 1, part_param%ndomain_eb(1)
            i = ix + (jk-1) * part_param%ndomain_eb(1)
!
            write(chara_tmp,'(a,a2)') trim(base_name), '_x'
            call add_index_after_name                                   &
     &         (ix, chara_tmp, domain_grp_name(i))
            write(chara_tmp,'(a,a2)') trim(domain_grp_name(i)), '_y'
            call add_index_after_name                                   &
     &         (iy, chara_tmp, domain_grp_name(i))
            write(chara_tmp,'(a,a2)') trim(domain_grp_name(i)), '_z'
            call add_index_after_name                                   &
     &         (iz, chara_tmp, domain_grp_name(i))
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_xyz_domain_grp_name
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_stacks_4_z_domain(my_rank, node, part_param,     &
     &          inod_sort, num_nod_group_z, istack_nod_grp_z)
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(volume_partioning_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
!
      integer(kind = kint), intent(in) :: num_nod_group_z
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_grp_z(0:num_nod_group_z)
!
      integer(kind = kint) :: iz, ist, ied, inum, inod
!
      do iz = 1, part_param%ndomain_eb(3)
        if(istack_nod_grp_z(iz) .eq. istack_nod_grp_z(iz-1)) cycle
!
        write(100+my_rank,*) 'istack_nod_grp_z', iz, istack_nod_grp_z(iz)
        ist = istack_nod_grp_z(iz-1) + 1
        ied = istack_nod_grp_z(iz  )
        do inum = ist, ied
          inod = inod_sort(inum)
          write(100+my_rank,*) 'inod', inum, inod, iz, node%xx(inod,3)
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_stacks_4_z_domain
!
! ----------------------------------------------------------------------
!
      subroutine check_stacks_4_yz_domain(my_rank, node, part_param,    &
     &          inod_sort, num_nod_grp_z, idomain_nod_grp_z,            &
     &          num_nod_group_yz, istack_nod_grp_y)
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(volume_partioning_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
!
      integer(kind = kint), intent(in) :: num_nod_grp_z
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_z(num_nod_grp_z)
      integer(kind = kint), intent(in) :: num_nod_group_yz
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_grp_y(0:num_nod_group_yz)
!
      integer(kind = kint) :: iy, iz, jk, ist, ied, inum, inod, icou
!
      do icou = 1, num_nod_grp_z
        iz = idomain_nod_grp_z(icou)
        jk = (iz-1) * part_param%ndomain_eb(2)
        write(100+my_rank,*) 'istack_nod_grp_y0', iz,                   &
     &                      istack_nod_grp_y(jk)
        do iy = 1, part_param%ndomain_eb(2)
          jk = iy + (iz-1) * part_param%ndomain_eb(2)
          write(100+my_rank,*) 'istack_nod_grp_y', iy, iz,              &
     &                        istack_nod_grp_y(jk)
          ist = istack_nod_grp_y(jk-1) + 1
          ied = istack_nod_grp_y(jk  )
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod,                    &
     &                           iy, iz, node%xx(inod,2:3)
          end do
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_stacks_4_yz_domain
!
! ----------------------------------------------------------------------
!
      subroutine check_stacks_4_new_domain(my_rank, node, part_param,   &
     &          inod_sort, num_nod_grp_yz, idomain_nod_grp_yz,          &
     &          num_nod_grp_xyz, istack_nod_grp_xyz)
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(volume_partioning_param), intent(in) :: part_param
      integer(kind = kint), intent(in) :: inod_sort(node%numnod)
!
      integer(kind = kint), intent(in) :: num_nod_grp_yz
      integer(kind = kint), intent(in)                                  &
     &     :: idomain_nod_grp_yz(num_nod_grp_yz)
      integer(kind = kint), intent(in) :: num_nod_grp_xyz
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_nod_grp_xyz(0:num_nod_grp_xyz)
!
      integer(kind = kint) :: ix, iy, iz, jk, ist, ied
      integer(kind = kint) :: inum, inod, icou, i
!
      do icou = 1, num_nod_grp_yz
        jk = idomain_nod_grp_yz(icou)
        iy = 1 + mod(jk-1,part_param%ndomain_eb(2))
        iz = 1 + (jk-1) / part_param%ndomain_eb(2)
        i = (jk-1) * part_param%ndomain_eb(1)
        write(100+my_rank,*) 'istack_nod_grp_x0',                       &
     &                        iy, iz, istack_nod_grp_xyz(i)
        do ix = 1, part_param%ndomain_eb(1)
          i = ix + (jk-1) * part_param%ndomain_eb(1)
          write(100+my_rank,*) 'istack_nod_grp_xyz', ix, iy, iz,        &
     &                          istack_nod_grp_xyz(i)
          ist = istack_nod_grp_xyz(i-1) + 1
          ied = istack_nod_grp_xyz(i  )
          do inum = ist, ied
            inod = inod_sort(inum)
            write(100+my_rank,*) 'inod', inum, inod,                    &
     &                            ix, iy, iz, node%xx(inod,1:3)
          end do
        end do
      end do
      write(100+my_rank,*) node%internal_node
!
      end subroutine check_stacks_4_new_domain
!
! ----------------------------------------------------------------------
!
      end module set_repartition_group_name
