!>@file   gen_sph_grids_modes.f90
!!@brief  module gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Serial version)
!!
!!@verbatim
!!      subroutine const_sph_rlm_modes                                  &
!!     &         (id_rank, gen_sph, sph_rlm, comm_rlm)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!      subroutine const_sph_rtm_grids                                  &
!!     &         (id_rank, gen_sph, sph_rtm, comm_rtm)
!!        type(construct_spherical_grid), intent(in) :: gen_sph
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!@endverbatim
!
      module gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      use t_const_spherical_grid
!
      implicit none
!
      private :: const_comm_table_4_rlm, const_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rlm_modes                                    &
     &         (id_rank, gen_sph, sph_rlm, comm_rlm)
!
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
!
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer, intent(in) :: id_rank
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
!
      call copy_gl_2_local_rlm_param(id_rank, gen_sph%s3d_ranks,        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, sph_rlm)
!
      call alloc_sph_1d_index_rlm(sph_rlm)
      call alloc_spheric_param_rlm(sph_rlm)
!
      call copy_sph_1d_gl_idx_rlm                                       &
     &   (gen_sph%s3d_radius, gen_sph%sph_gl1d, sph_rlm)
!
      if(gen_sph%s3d_ranks%rlm_rin_flag) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'set_global_sph_4_rlm', id_rank
        call set_global_sph_4_rlm(sph_rlm)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'set_global_sph_4_lmr', id_rank
        call set_global_sph_4_lmr(sph_rlm)
      end if
!
      if(iflag_debug .gt. 0) then
        call check_spheric_param_rlm(id_rank, sph_rlm)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rlm', id_rank
      call const_comm_table_4_rlm                                       &
     &   (id_rank, gen_sph%s3d_ranks, sph_rlm, comm_rlm)
!
      end subroutine const_sph_rlm_modes
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_rtm_grids                                    &
     &         (id_rank, gen_sph, sph_rtm, comm_rtm)
!
      use t_spheric_rtm_data
      use t_sph_trans_comm_tbl
!
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
!
      integer, intent(in) :: id_rank
      type(construct_spherical_grid), intent(in) :: gen_sph
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
!
      call copy_gl_2_local_rtm_param(id_rank, gen_sph%s3d_ranks,        &
     &    gen_sph%sph_lcp, gen_sph%stk_lc1d, sph_rtm)
!
      call alloc_spheric_param_rtm(sph_rtm)
      call alloc_sph_1d_index_rtm(sph_rtm)
!
      call copy_sph_1d_gl_idx_rtm(gen_sph%s3d_radius,                   &
     &                            gen_sph%sph_gl1d, sph_rtm)
!
      if(gen_sph%s3d_ranks%rtm_rin_flag) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'set_global_sph_4_rtm', id_rank
        call set_global_sph_4_rtm(sph_rtm)
      else
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'set_global_sph_4_trm', id_rank
        call set_global_sph_4_trm(sph_rtm)
      end if
!
      if(iflag_debug .gt. 0) then
        call check_spheric_param_rtm(id_rank, sph_rtm)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'const_comm_table_4_rtm', id_rank
      call const_comm_table_4_rtm                                       &
     &   (id_rank, gen_sph%s3d_ranks, sph_rtm, comm_rtm)
!
      end subroutine const_sph_rtm_grids
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rlm                                 &
     &         (id_rank, s3d_ranks, sph_rlm, comm_rlm)
!
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use set_comm_table_rtm_rlm
!
      integer, intent(in) :: id_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
!
!
      call allocate_ncomm(s3d_ranks%ndomain_sph)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'count_comm_table_4_rlm', id_rank
      call count_comm_table_4_rlm                                       &
     &   (s3d_ranks, sph_rlm%nnod_rlm, sph_rlm%idx_global_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'count_num_domain_rtm_rlm', id_rank
      call count_num_domain_rtm_rlm                                     &
     &   (s3d_ranks%ndomain_sph, comm_rlm%nneib_domain)
!
      call alloc_sph_comm_stack(comm_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_comm_stack_rtm_rlm', id_rank
      call set_comm_stack_rtm_rlm(id_rank, s3d_ranks%ndomain_sph,       &
     &    comm_rlm%nneib_domain, comm_rlm%id_domain,                    &
     &    comm_rlm%istack_sr, comm_rlm%ntot_item_sr)
!
      call alloc_sph_comm_item(sph_rlm%nnod_rlm, comm_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &          'set_comm_table_4_rlm', id_rank
      call set_comm_table_4_rlm                                         &
     &   (s3d_ranks, sph_rlm%nnod_rlm, sph_rlm%idx_global_rlm,          &
     &    comm_rlm%nneib_domain, comm_rlm%ntot_item_sr,                 &
     &    comm_rlm%istack_sr, comm_rlm%item_sr)
      call deallocate_ncomm
!
!      call allocate_idx_gl_rlm_out
!      call set_global_id_4_comm_rlm
!
      end subroutine const_comm_table_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtm                                 &
     &         (id_rank, s3d_ranks, sph_rtm, comm_rtm)
!
      use t_spheric_rtm_data
      use t_sph_trans_comm_tbl
      use set_comm_table_rtm_rlm
!
      integer, intent(in) :: id_rank
      type(spheric_global_rank), intent(in) :: s3d_ranks
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm
!
!
!      write(*,*) 'allocate_ncomm'
      call allocate_ncomm(s3d_ranks%ndomain_sph)
!
!      write(*,*) 'count_comm_table_4_rtm'
      call count_comm_table_4_rtm(s3d_ranks, sph_rtm%nnod_rtm,          &
     &    sph_rtm%nidx_global_rtm, sph_rtm%idx_global_rtm)
!
!      write(*,*) 'count_num_domain_rtm_rlm'
      call count_num_domain_rtm_rlm                                     &
     &   (s3d_ranks%ndomain_sph, comm_rtm%nneib_domain)
!
!      write(*,*) 'alloc_sph_comm_stack'
      call alloc_sph_comm_stack(comm_rtm)
!
!      write(*,*) 'set_comm_stack_rtm_rlm'
      call set_comm_stack_rtm_rlm(id_rank, s3d_ranks%ndomain_sph,       &
     &    comm_rtm%nneib_domain, comm_rtm%id_domain,                    &
     &    comm_rtm%istack_sr, comm_rtm%ntot_item_sr)
!
      call alloc_sph_comm_item(sph_rtm%nnod_rtm, comm_rtm)
!
!      write(*,*) 'set_comm_table_4_rtm'
      call set_comm_table_4_rtm(s3d_ranks, sph_rtm%nnod_rtm,            &
     &    sph_rtm%nidx_global_rtm, sph_rtm%idx_global_rtm,              &
     &    comm_rtm%nneib_domain, comm_rtm%ntot_item_sr,                 &
     &    comm_rtm%istack_sr, comm_rtm%item_sr)
      call deallocate_ncomm
!
!      call allocate_idx_gl_rtm_out
!      write(*,*) 'set_global_id_4_comm_rtm'
!      call set_global_id_4_comm_rtm
!
      end subroutine const_comm_table_4_rtm
!
! -----------------------------------------------------------------------
!
      end module gen_sph_grids_modes
