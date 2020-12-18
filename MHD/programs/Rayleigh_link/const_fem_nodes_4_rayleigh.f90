!>@file   const_fem_nodes_4_rayleigh.f90
!!@brief  module const_fem_nodes_4_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine output_fem_nodes_4_rayleigh(mesh_file, rayleigh_rtp)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!
!!      subroutine s_const_fem_nodes_4_rayleigh                         &
!!     &         (rayleigh_rtp, mesh, group)
!!      subroutine fem_nodes_4_rayleigh_file(rayleigh_rtp, mesh, group)
!!        type(rayleigh_field), intent(in) :: rayleigh_rtp
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!      subroutine shell_params_from_rayleigh                           &
!!     &         (rayleigh_rtp, sph, gen_sph)
!!        type(rayleigh_field), intent(in) :: rayleigh_rtp
!!        type(sph_grids), intent(inout) :: sph
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!@endverbatim
!
      module const_fem_nodes_4_rayleigh
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      use calypso_mpi
!
      use t_rayleigh_field_IO
      use t_mesh_data
      use t_group_data
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &       ::  rayleigh_resolution_file = 'Rayleigh_grid_kemo.dat'
!
      private :: nodes_4_rayleigh, nodes_4_rayleigh_file
      private :: const_ele_and_grp_4_rayleigh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_fem_nodes_4_rayleigh(mesh_file, rayleigh_rtp)
!
      use t_file_IO_parameter
      use mpi_load_mesh_data
      use palallel_conf_4_rayleigh
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
      type(mesh_geometry) :: mesh0
      type(mesh_groups) ::   group0
!
!
      call load_resolution_4_rayleigh                                   &
     &   (rayleigh_resolution_file, rayleigh_rtp)
      call s_const_fem_nodes_4_rayleigh(rayleigh_rtp, mesh0, group0)
!
      call mpi_output_mesh(mesh_file, mesh0, group0)
!
      call dealloc_groups_data(group0)
      call dealloc_mesh_geometry_base(mesh0)
!
      end subroutine output_fem_nodes_4_rayleigh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_const_fem_nodes_4_rayleigh                           &
     &         (rayleigh_rtp, mesh, group)
!
      type(rayleigh_field), intent(in) :: rayleigh_rtp
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      mesh%node%numnod = (rayleigh_rtp%ked - rayleigh_rtp%kst + 1)      &
     &                  * (rayleigh_rtp%led - rayleigh_rtp%lst + 1)     &
     &                  * rayleigh_rtp%nphi_gl
      mesh%node%internal_node = mesh%node%numnod
      call alloc_node_geometry_base(mesh%node)
!
      call nodes_4_rayleigh(rayleigh_rtp, mesh%node)
!
      call const_ele_and_grp_4_rayleigh(mesh, group)
!
      end subroutine s_const_fem_nodes_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine fem_nodes_4_rayleigh_file(rayleigh_rtp, mesh, group)
!
      use cal_minmax_and_stacks
!
      type(rayleigh_field), intent(in) :: rayleigh_rtp
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
      integer(kind = kint), allocatable :: istack_nnod(:)
      integer(kind = kint) :: nnod_gl, ndivideed, irest, istart_pe
!
!
      nnod_gl = rayleigh_rtp%nri_gl * rayleigh_rtp%nth_gl              &
     &         * rayleigh_rtp%nphi_gl
!
      allocate(istack_nnod(0:nprocs))
      call cal_divide_and_rest(ndivideed, irest, nnod_gl, nprocs)
      call set_stack_of_segments                                       &
     &   (nprocs, ndivideed, irest, ione, istack_nnod)
!
      istart_pe = istack_nnod(my_rank)
      mesh%node%numnod = istack_nnod(my_rank+1) -  istack_nnod(my_rank)
      mesh%node%internal_node = mesh%node%numnod
      deallocate(istack_nnod)
!
      call alloc_node_geometry_base(mesh%node)
!
!      write(*,*) 'nodes_4_rayleigh_file'
      call nodes_4_rayleigh_file(istart_pe, rayleigh_rtp, mesh%node)
!
      call const_ele_and_grp_4_rayleigh(mesh, group)
!
      end subroutine fem_nodes_4_rayleigh_file
!
! ----------------------------------------------------------------------
!
      subroutine const_ele_and_grp_4_rayleigh(mesh, group)
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
!
      mesh%ele%numele = 0
      mesh%ele%nnod_4_ele = num_t_linear
      call alloc_ele_connect(mesh%ele)
!
!      write(*,*) 'empty_comm_table'
      call empty_comm_table(mesh%nod_comm)
!
!
      group%nod_grp%num_grp = 0
      call alloc_group_num(group%nod_grp)
      call alloc_group_item(group%nod_grp)
!
      group%ele_grp%num_grp = 0
      call alloc_group_num(group%ele_grp)
      call alloc_group_item(group%ele_grp)
!
      group%surf_grp%num_grp = 0
      call alloc_sf_group_num(group%surf_grp)
      call alloc_sf_group_item(group%surf_grp)
!
      end subroutine const_ele_and_grp_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine nodes_4_rayleigh(rayleigh_rtp, node)
!
      use coordinate_converter
!
      type(rayleigh_field), intent(in) :: rayleigh_rtp
      type(node_data), intent(inout) :: node
!
      integer(kind = kint) :: k, l, m, k_to_out, inod
      real(kind = kreal) :: pi
      real(kind = kreal), allocatable :: r(:), t(:), p(:)
!
!
      pi = four * atan(one)
      allocate(r(node%numnod), t(node%numnod), p(node%numnod))
!
!$omp parallel do private(k,l,m,k_to_out,inod)
      do k = rayleigh_rtp%kst, rayleigh_rtp%ked
        inod = (k - rayleigh_rtp%kst) * rayleigh_rtp%nphi_gl            &
     &        * (rayleigh_rtp%led - rayleigh_rtp%lst + 1)
        do l = rayleigh_rtp%lst, rayleigh_rtp%led
          do m = 1, rayleigh_rtp%nphi_gl
            k_to_out = rayleigh_rtp%nri_gl - k + 1
            inod = inod + 1
            node%inod_global(inod)  &
     &             = k_to_out + (l-1) * rayleigh_rtp%nri_gl             &
     &              + (m-1) * rayleigh_rtp%nth_gl * rayleigh_rtp%nri_gl
            r(inod) = rayleigh_rtp%radius_gl(k)
            t(inod) = rayleigh_rtp%theta_gl(l)
            p(inod) = two * pi * dble(m-1) / dble(rayleigh_rtp%nphi_gl)
          end do
        end do
      end do
!$omp end parallel do
!
      call position_2_xyz(node%numnod, r, t, p,                         &
     &    node%xx(1,1), node%xx(1,2), node%xx(1,3))
      deallocate(r, t, p)
!
      end subroutine nodes_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine nodes_4_rayleigh_file(ist_pe, rayleigh_rtp, node)
!
      use coordinate_converter
!
      integer(kind = kint), intent(in) :: ist_pe
      type(rayleigh_field), intent(in) :: rayleigh_rtp
      type(node_data), intent(inout) :: node
!
      integer(kind = kint) :: k, l, m, k_to_out, inod, l_tmp, i_gl
      real(kind = kreal) :: pi
      real(kind = kreal), allocatable :: r(:), t(:), p(:)
!
!
      pi = four * atan(one)
      allocate(r(node%numnod), t(node%numnod), p(node%numnod))
!
!$omp parallel do private(i_gl,k,l,m,l_tmp,k_to_out,inod)
      do inod = 1, node%numnod
        i_gl = inod + ist_pe
        m = mod(i_gl-1,rayleigh_rtp%nphi_gl)
        l_tmp = (i_gl-m) / rayleigh_rtp%nphi_gl
        l = mod(l_tmp,rayleigh_rtp%nth_gl)
        k = (l_tmp-l) / rayleigh_rtp%nth_gl
        m = m + 1
        l = l + 1
        k = k + 1
        k_to_out = rayleigh_rtp%nri_gl - k + 1
        node%inod_global(inod)                                          &
     &             = k_to_out + (l-1) * rayleigh_rtp%nri_gl             &
     &              + (m-1) * rayleigh_rtp%nth_gl * rayleigh_rtp%nri_gl
        r(inod) = rayleigh_rtp%radius_gl(k)
        t(inod) = rayleigh_rtp%theta_gl(l)
        p(inod) = two * pi * dble(m-1) / dble(rayleigh_rtp%nphi_gl)
      end do
!$omp end parallel do
!
      call position_2_xyz(node%numnod, r, t, p,                         &
     &    node%xx(1,1), node%xx(1,2), node%xx(1,3))
      deallocate(r, t, p)
!
      end subroutine nodes_4_rayleigh_file
!
! ----------------------------------------------------------------------
!
      subroutine shell_params_from_rayleigh                             &
     &         (rayleigh_rtp, sph, gen_sph)
!
      use t_mesh_data
      use t_spheric_parameter
      use t_const_spherical_grid
      use cal_minmax_and_stacks
!
      type(rayleigh_field), intent(in) :: rayleigh_rtp
!
      type(sph_grids), intent(inout) :: sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      integer(kind = kint) :: i, irev
!
!
      sph%sph_params%iflag_shell_mode = iflag_MESH_same
!
      call sph_rj_params_from_rayleigh(rayleigh_rtp, sph%sph_rj)
      call sph_rtp_params_from_rayleigh(rayleigh_rtp, sph%sph_rtp)
!
      call radial_rtp_grp_from_rayleigh                                 &
     &   (sph%sph_rj, sph%sph_rtp, gen_sph%radial_rtp_grp_lc)
      call empty_theta_rtp_grp(gen_sph%theta_rtp_grp_lc)
      call radial_rj_grp_from_rayleigh                                  &
     &   (sph%sph_rj, gen_sph%radial_rj_grp_lc)
!
      end subroutine shell_params_from_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine sph_rj_params_from_rayleigh(rayleigh_rtp, sph_rj)
!
      use t_spheric_rj_data
!
      type(rayleigh_field), intent(in) :: rayleigh_rtp
!
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: i, irev
!
!
      sph_rj%nidx_global_rj(1) = rayleigh_rtp%nri_gl
      sph_rj%nidx_global_rj(2) = (rayleigh_rtp%ltr + 1)**2
      sph_rj%nidx_rj(1) = sph_rj%nidx_global_rj(1)
      sph_rj%nidx_rj(2) = sph_rj%nidx_global_rj(2) / nprocs
!
      call alloc_sph_1d_index_rj(sph_rj)
      do i = 1, rayleigh_rtp%nri_gl
        irev = rayleigh_rtp%nri_gl - i + 1
        sph_rj%radius_1d_rj_r(i) = rayleigh_rtp%radius_gl(irev)
      end do
!
      end subroutine sph_rj_params_from_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine sph_rtp_params_from_rayleigh(rayleigh_rtp, sph_rtp)
!
      use t_spheric_rtp_data
!
      type(rayleigh_field), intent(in) :: rayleigh_rtp
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      integer(kind = kint) :: i
!
!
      sph_rtp%irank_sph_rtp(1) = rayleigh_rtp%irank_r
      sph_rtp%irank_sph_rtp(2) = rayleigh_rtp%irank_h
      sph_rtp%irank_sph_rtp(3) = 0

      sph_rtp%nidx_global_rtp(1) = rayleigh_rtp%nri_gl
      sph_rtp%nidx_global_rtp(2) = rayleigh_rtp%nth_gl
      sph_rtp%nidx_global_rtp(3) = rayleigh_rtp%nphi_gl
      sph_rtp%ist_rtp(1) = rayleigh_rtp%kst
      sph_rtp%ist_rtp(2) = rayleigh_rtp%lst
      sph_rtp%ist_rtp(3) = 1
      sph_rtp%ied_rtp(1) = rayleigh_rtp%ked
      sph_rtp%ied_rtp(2) = rayleigh_rtp%led
      sph_rtp%ied_rtp(3) = rayleigh_rtp%nphi_gl
      sph_rtp%nidx_rtp(1) = rayleigh_rtp%ked - rayleigh_rtp%kst + 1
      sph_rtp%nidx_rtp(2) = rayleigh_rtp%led - rayleigh_rtp%lst + 1
      sph_rtp%nidx_rtp(3) = rayleigh_rtp%nphi_gl
!
      call alloc_sph_1d_index_rtp(sph_rtp)
!
      do i = 1, sph_rtp%nidx_rtp(1)
        sph_rtp%idx_gl_1d_rtp_r(i) = sph_rtp%ist_rtp(1) + i- 1
      end do
!
      end subroutine sph_rtp_params_from_rayleigh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine radial_rtp_grp_from_rayleigh                           &
     &         (sph_rj, sph_rtp, radial_rtp_grp_lc)
!
      use t_spheric_rj_data
      use t_spheric_rtp_data
      use t_group_data
      use cal_minmax_and_stacks
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(group_data), intent(inout) :: radial_rtp_grp_lc
!
      integer(kind = kint) :: i
!
!
      radial_rtp_grp_lc%num_grp = 5
      call alloc_group_num(radial_rtp_grp_lc)
!
      radial_rtp_grp_lc%grp_name(1) = 'ICB'
      radial_rtp_grp_lc%grp_name(2) = 'CMB'
      radial_rtp_grp_lc%grp_name(3) = 'to_Center'
      radial_rtp_grp_lc%grp_name(4) = 'inner_core'
      radial_rtp_grp_lc%grp_name(5) = 'outer_core'
!
      radial_rtp_grp_lc%nitem_grp(1:5) = 0
      if(sph_rtp%ist_rtp(1) .eq. 1) then
        radial_rtp_grp_lc%nitem_grp(1) = 1
        radial_rtp_grp_lc%nitem_grp(3) = 1
      end if
      if(sph_rtp%ied_rtp(1) .eq. sph_rj%nidx_rj(1)) then
        radial_rtp_grp_lc%nitem_grp(2) = 1
      end if
      radial_rtp_grp_lc%nitem_grp(4) = 0
      radial_rtp_grp_lc%nitem_grp(5) = sph_rtp%ied_rtp(1)               &
     &                                - sph_rtp%ist_rtp(1) + 1
!
      call s_cal_total_and_stacks                                       &
     &  (radial_rtp_grp_lc%num_grp, radial_rtp_grp_lc%nitem_grp, izero, &
     &   radial_rtp_grp_lc%istack_grp, radial_rtp_grp_lc%num_item)
      call alloc_group_item(radial_rtp_grp_lc)
!
      if(sph_rtp%ist_rtp(1) .eq. 1) then
        i = radial_rtp_grp_lc%istack_grp(0) + 1
        radial_rtp_grp_lc%nitem_grp(i) = 1
      end if
      if(sph_rtp%ied_rtp(1) .eq. sph_rj%nidx_rj(1)) then
        i = radial_rtp_grp_lc%istack_grp(1) + 1
        radial_rtp_grp_lc%nitem_grp(i) = sph_rtp%ied_rtp(1)
      end if
      if(sph_rtp%ist_rtp(1) .eq. 1) then
        i = radial_rtp_grp_lc%istack_grp(2) + 1
        radial_rtp_grp_lc%nitem_grp(i) = 1
      end if
!
      do i = 1, sph_rtp%nidx_rtp(1)
        radial_rtp_grp_lc%item_grp(i+3) = i
      end do
!
      end subroutine radial_rtp_grp_from_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine radial_rj_grp_from_rayleigh(sph_rj, radial_rj_grp_lc)
!
      use t_spheric_rj_data
      use t_group_data
      use cal_minmax_and_stacks
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(inout) :: radial_rj_grp_lc
!
      integer(kind = kint) :: i
!
!
      radial_rj_grp_lc%num_grp =  5
      call alloc_group_num(radial_rj_grp_lc)
      radial_rj_grp_lc%grp_name(1) = 'ICB'
      radial_rj_grp_lc%grp_name(2) = 'CMB'
      radial_rj_grp_lc%grp_name(3) = 'to_Center'
      radial_rj_grp_lc%grp_name(4) = 'inner_core'
      radial_rj_grp_lc%grp_name(5) = 'outer_core'
!
      radial_rj_grp_lc%nitem_grp(1) = 1
      radial_rj_grp_lc%nitem_grp(2) = 1
      radial_rj_grp_lc%nitem_grp(3) = 1
      radial_rj_grp_lc%nitem_grp(4) = 0
      radial_rj_grp_lc%nitem_grp(5) = sph_rj%nidx_rj(1)
!
      call s_cal_total_and_stacks                                       &
     &   (radial_rj_grp_lc%num_grp, radial_rj_grp_lc%nitem_grp, izero,  &
     &    radial_rj_grp_lc%istack_grp, radial_rj_grp_lc%num_item)
      call alloc_group_item(radial_rj_grp_lc)
!
      radial_rj_grp_lc%item_grp(1) = 1
      radial_rj_grp_lc%item_grp(2) = sph_rj%nidx_rj(1)
      radial_rj_grp_lc%item_grp(3) = 1
      do i = 1, sph_rj%nidx_rj(1)
        radial_rj_grp_lc%item_grp(i+3) = i
      end do
!
      end subroutine radial_rj_grp_from_rayleigh
!
! -----------------------------------------------------------------------
!
      end module const_fem_nodes_4_rayleigh
