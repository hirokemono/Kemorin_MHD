!
!      module devide_by_spherical_coord
!
!      Written by H. Matsui
!
!      subroutine divide_by_sphere_coord(nproc, nnod, xx,               &
!     &          radius, theta, phi, num_bc_grp, ntot_bc_grp,           &
!     &          istack_bc_grp, item_bc_grp, name_bc_grp)
!        output: IGROUP_nod
!
      module devide_by_spherical_coord
!
      use m_precision
!
      implicit none
!
      real(kind=kreal), allocatable :: VAL_sph(:)
      integer(kind=kint), allocatable :: IS_sph(:)
!
      real(kind=kreal), allocatable :: VAL_cube(:)
      integer(kind=kint), allocatable :: IS_cube(:)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine divide_by_sphere_coord(nproc, nnod, nnod_4_ele, xx,    &
     &          radius, theta, phi, num_bc_grp, ntot_bc_grp,            &
     &          istack_bc_grp, item_bc_grp, name_bc_grp)
!
      use m_shell_surface
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
!
      use sort_sphere_4_rcb
      use sort_cube_4_rcb
      use cal_minmax_and_stacks
      use find_shell_information
      use grouping_by_sphere
      use set_sphere_data
!
      integer(kind = kint), intent(in)  :: nproc, nnod, nnod_4_ele
      real(kind= kreal), intent(in) :: xx(nnod,3)
      real(kind= kreal), intent(in) :: radius(nnod)
      real(kind= kreal), intent(in) :: theta(nnod)
      real(kind= kreal), intent(in) :: phi(nnod)
!
      integer(kind = kint), intent(in) :: num_bc_grp, ntot_bc_grp
      integer(kind = kint), intent(in) :: istack_bc_grp(0:num_bc_grp)
      integer(kind = kint), intent(in) :: item_bc_grp(ntot_bc_grp)
      character(len=kchara), intent(in) :: name_bc_grp(num_bc_grp)
!
      integer(kind = kint)  :: irest1, num1
!
!C
!C +------------------------------+
!C | RCB for spherical coordinate |
!C +------------------------------+
!C===
!
      call allocate_local_nnod_shell(nproc)
!
      call cal_divide_and_rest(num1, irest1, nnod, nproc)
      call set_number_of_segments(nproc, num1, irest1, numnod_local)
!
!
        if (nnod_4_ele.ne.8 .and. iflag_sphere_data.eq.0) then
          write(*,*) 'Sphere correction data is required!'
          stop
        end if
        if (ndivide_eb(1).gt.1 .and. iflag_sphere_data.eq.0) then
          write(*,*) 'Use sphere correction data.'
          stop
        end if
!
        if (iflag_sphere_data.eq. 0) then
          call s_find_shell_information(ndivide_eb(2), ndivide_eb(3),   &
     &        nnod, radius, theta, phi, num_bc_grp, ntot_bc_grp,        &
     &        istack_bc_grp, item_bc_grp, name_bc_grp)
        else
          if (nnod_4_ele .eq. 8) then
            call set_sphere_data_4_linear(ndivide_eb(2), ndivide_eb(3))
          else if (nnod_4_ele .eq. 20) then
            call set_sphere_data_4_quad(ndivide_eb(2), ndivide_eb(3))
          end if
        end if
!
      allocate (VAL_sph(num_CMB))
      allocate (IS_sph(num_CMB))

      allocate (VAL_cube(nnod))
      allocate (IS_cube(nnod))

      IGROUP_nod(1:nnod)= 1

!   grouping radial layer
!
        call part_sphere_with_radius(ndivide_eb(1), IGROUP_radius,      &
     &          num_layer, nlayer_ICB, nlayer_CMB)
!
!   grouping nodes on sphere
!
        write(*,*) 's_sort_sphere_4_rcb'
        call s_sort_sphere_4_rcb(num_CMB, IGROUP_cmb, ndivide_eb(2),    &
     &      ndivide_eb(3), rtp_cmb(1,2), rtp_cmb(1,3), VAL_sph, IS_sph)
!
!       write(*,*) 'IGROUP_cmb', IGROUP_cmb
!   grouping nodes in shell
!
        write(*,*) 'set_sphere_domain_list_l'
        call set_sphere_domain_list_l(nproc, nnod, ndivide_eb(1),       &
     &          num_CMB, nnod_CMB, num_layer, istack_sph, item_sph,     &
     &          IGROUP_cmb, IGROUP_radius, IGROUP_nod, ncore_local)
!
        if (nnod_4_ele .eq. 20) then
          write(*,*) 'set_sphere_domain_list_q'
          call set_sphere_domain_list_q(nproc, nnod, ndivide_eb(1),     &
     &          num_CMB, num_layer, istack20_sph, item20_sph,           &
     &          IGROUP_cmb, IGROUP_radius, IGROUP_nod, ncore_local)
        end if
!
!   count nuber of node for each subdomain
!
        write(*,*) 'count_num_subdomain_4_cube'
        call count_num_subdomain_4_cube(nproc,                          &
     &          numnod_local, ncore_local, nrest_local)
!
!  devide around center
!
        call s_sort_cube_4_rcb(nnod, num_cube, nproc, ndivide_eb(1),    &
     &      ndivide_eb(2), ndivide_eb(3), xx, phi, inod_free,           &
     &      nrest_local, IGROUP_nod, IS_cube, VAL_cube)
!
      deallocate (VAL_sph, IS_sph)
      deallocate (VAL_cube, IS_cube)

      end subroutine divide_by_sphere_coord
!
!   --------------------------------------------------------------------
!
      end module devide_by_spherical_coord
