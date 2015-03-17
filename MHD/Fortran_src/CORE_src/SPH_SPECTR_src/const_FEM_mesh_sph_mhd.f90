!
      module const_FEM_mesh_sph_mhd
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_sph_mhd(my_rank, mesh, group)
!
      use t_mesh_data
      use t_group_data
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_gauss_points
      use set_FEM_mesh_4_sph
      use const_global_sph_grids_modes
      use const_1d_ele_connect_4_sph
      use set_sph_groups
!
      integer(kind = kint), intent(in) :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
!
!
      call allocate_gauss_points(nidx_global_rtp(2))
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call s_const_global_sph_grids_modes
      call s_const_1d_ele_connect_4_sph
!
      call copy_gl_2_local_rtp_fem(my_rank)
      call s_const_FEM_mesh_for_sph(my_rank, mesh, group)
!
      call deallocate_nidx_local
!
      call deallocate_nnod_nele_sph_mesh
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine const_FEM_mesh_4_sph_mhd
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_gl_2_local_rtp_fem(my_rank)
!
      use m_spheric_parameter
      use m_sph_global_parameter
      use m_spheric_global_ranks
      use m_sph_1d_global_index
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i1, i2, i3
!
!
      write(*,*) 'sph_rank_rtp org', sph_rank_rtp
      write(*,*) 'nnod_rtp org', nnod_rtp
      write(*,*) 'nidx_rtp org', nidx_rtp
      write(*,*) 'ist_rtp org', ist_rtp
      write(*,*) 'ied_rtp org', ied_rtp
      write(*,*) 'nidx_local_fem org', nidx_local_fem
      write(*,*) 'nidx_local_fem org', nidx_local_fem
      sph_rank_rtp(1:3) = iglobal_rank_rtp(1:3,my_rank)
!
      nnod_rtp = nnod_local_rtp(my_rank+1)
!
      nidx_rtp(1:3) = nidx_local_rtp(my_rank+1,1:3)
!
      i1 = sph_rank_rtp(1) + 1
      i2 = sph_rank_rtp(2) + 1
      i3 = sph_rank_rtp(3) + 1
      ist_rtp(1) = istack_idx_local_rtp_r(i1-1) + 1
      ist_rtp(2) = istack_idx_local_rtp_t(i2-1) + 1
      ist_rtp(3) = istack_idx_local_rtp_p(i3-1) + 1
      ied_rtp(1) = istack_idx_local_rtp_r(i1)
      ied_rtp(2) = istack_idx_local_rtp_t(i2)
      ied_rtp(3) = istack_idx_local_rtp_p(i3)
!
      nidx_local_fem(1:3) = nidx_rtp(1:3)
      nidx_local_fem(3) =   m_folding * nidx_local_fem(3)
!
      write(*,*) 'sph_rank_rtp new', sph_rank_rtp
      write(*,*) 'nnod_rtp  new', nnod_rtp
      write(*,*) 'nidx_rtp  new', nidx_rtp
      write(*,*) 'ist_rtp  new', ist_rtp
      write(*,*) 'ied_rtp  new', ied_rtp
      write(*,*) 'nidx_local_fem  new', nidx_local_fem
      write(*,*) 'nidx_local_fem  new', nidx_local_fem
!
      end subroutine copy_gl_2_local_rtp_fem
!
! -----------------------------------------------------------------------
!
      end module const_FEM_mesh_sph_mhd
