!>@file   set_comm_table_rtp_rj.f90
!!@brief  module set_comm_table_rtp_rj
!!
!!@author  H. Matsui
!!@date Programmed on July, 2007
!
!
!> @brief Construct communication table for rj and rtp grid
!!
!!@verbatim
!!      subroutine const_sph_rj_modes(ip_rank, ndomain_sph, comm_rlm)
!!      subroutine const_sph_rtp_grids(ip_rank, ndomain_sph, comm_rtm)
!!
!!      subroutine set_comm_stack_rtp_rj(nneib_domain, id_domain,       &
!!     &          istack_sr, ntot_item_sr)
!!@endverbatim
!
      module set_comm_table_rtp_rj
!
      use m_precision
      use m_machine_parameter
      use t_sph_trans_comm_tbl
!
      implicit none
!
      integer(kind = kint), allocatable :: id_domain_tmp(:)
      integer(kind = kint), allocatable :: nnod_sr_tmp(:)
      private :: id_domain_tmp, nnod_sr_tmp
!
      private :: allocate_domain_sr_tmp,  deallocate_domain_sr_tmp
      private :: const_comm_table_4_rj,  const_comm_table_4_rtp
      private :: count_comm_table_4_rj,  set_comm_table_4_rj
      private :: count_comm_table_4_rtp, set_comm_table_4_rtp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_domain_sr_tmp(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
!
      allocate( id_domain_tmp(ndomain_sph) )
      allocate( nnod_sr_tmp(ndomain_sph) )
      id_domain_tmp = 0
      nnod_sr_tmp = 0
!
      end subroutine allocate_domain_sr_tmp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_domain_sr_tmp
!
      deallocate( id_domain_tmp )
      deallocate( nnod_sr_tmp )
!
      end subroutine deallocate_domain_sr_tmp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_sph_rj_modes(ip_rank, ndomain_sph, comm_rlm)
!
      use m_spheric_parameter
!
      use t_spheric_rj_data
!
      use load_data_for_sph_IO
      use set_sph_groups
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
      use set_local_index_table_sph
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm(ndomain_sph)
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'copy_gl_2_local_rj_param', ip_rank
      call copy_gl_2_local_rj_param(ip_rank)
!
      call add_center_mode_rj
!
      sph_rj1%nnod_rj = nnod_rj
      sph_rj1%nidx_rj(1:2) = nidx_rj(1:2)
      call alloc_type_spheric_param_rj(sph_rj1)
      call alloc_type_sph_1d_index_rj(sph_rj1)
!
      call copy_sph_1d_gl_idx_rj
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'set_global_sph_rj_id', ip_rank
      call set_global_sph_rj_id
!
      if(iflag_debug .gt. 0) then
        call check_type_spheric_param_rj(ip_rank, sph_rj1)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rj', ip_rank
      call const_comm_table_4_rj                                        &
     &   (ip_rank, nnod_rj, ndomain_sph, comm_rlm)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                  'set_sph_rj_groups', ip_rank
      call set_sph_rj_groups
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'output_modes_rj_sph_trans', ip_rank
      call output_modes_rj_sph_trans(ip_rank, l_truncation, sph_rj1)
!
      write(*,'(a,i6,a)') 'Spherical modes for domain',                 &
     &          ip_rank, ' is done.'
!
      end subroutine const_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine const_sph_rtp_grids(ip_rank, ndomain_sph, comm_rtm)
!
      use m_spheric_parameter
!
      use t_spheric_rtp_data
!
      use load_data_for_sph_IO
      use set_sph_groups
      use copy_sph_1d_global_index
      use set_local_sphere_param
      use set_local_sphere_by_global
      use set_local_index_table_sph
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm(ndomain_sph)
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                'copy_gl_2_local_rtp_param', ip_rank
      call copy_gl_2_local_rtp_param(ip_rank)
!
      sph_rtp1%nnod_rtp = nnod_rtp
      sph_rtp1%nidx_rtp(1:3) = nidx_rtp(1:3)
      call alloc_type_spheric_param_rtp(sph_rtp1)
      call alloc_type_sph_1d_index_rtp(sph_rtp1)
!
      call copy_sph_1d_gl_idx_rtp
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'set_global_sph_rtp_id', ip_rank
      call set_global_sph_rtp_id
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'check_spheric_param_rtp', ip_rank
        call check_type_spheric_param_rtp(ip_rank, sph_rtp1)
      end if
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'const_comm_table_4_rtp', ip_rank
      call const_comm_table_4_rtp                                       &
     &   (ip_rank, nnod_rtp, ndomain_sph, comm_rtm)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_rtp_groups', ip_rank
      call set_sph_rtp_groups
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &                 'output_geom_rtp_sph_trans', ip_rank
      call output_geom_rtp_sph_trans(ip_rank, l_truncation, sph_rtp1)
!
      write(*,'(a,i6,a)') 'Spherical grids for domain',                 &
     &          ip_rank, ' is done.'
!
      end subroutine const_sph_rtp_grids
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rj                                  &
     &         (ip_rank, nnod_rj, ndomain_sph, comm_rlm)
!
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ip_rank, nnod_rj
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm(ndomain_sph)
      integer(kind = kint) :: icou
!
!
      call allocate_domain_sr_tmp(ndomain_sph)
!
      nneib_domain_rj = 0
      call count_comm_table_4_rj(ip_rank, ndomain_sph, comm_rlm)
!
      call allocate_sph_comm_stack_rj
!
      call set_comm_stack_rtp_rj(nneib_domain_rj, id_domain_rj,         &
     &    istack_sr_rj, ntot_item_sr_rj)
!
      call deallocate_domain_sr_tmp
      call allocate_sph_comm_item_rj(nnod_rj)
!
      icou = 0
      call set_comm_table_4_rj(ip_rank, ndomain_sph, comm_rlm, icou)
!
      end subroutine const_comm_table_4_rj
!
! -----------------------------------------------------------------------
!
      subroutine const_comm_table_4_rtp                                 &
     &         (ip_rank, nnod_rtp, ndomain_sph, comm_rtm)
!
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm(ndomain_sph)
!
      integer(kind = kint) :: icou
!
!
      call allocate_domain_sr_tmp(ndomain_sph)
!
      call count_comm_table_4_rtp(ip_rank, ndomain_sph, comm_rtm,       &
     &    comm_rtp1%nneib_domain)
!
      call alloc_type_sph_comm_stack(comm_rtp1)
!
      call set_comm_stack_rtp_rj                                        &
     &   (comm_rtp1%nneib_domain, comm_rtp1%id_domain,                  &
     &    comm_rtp1%istack_sr, comm_rtp1%ntot_item_sr)
!      write(*,*) 'nneib_domain_rtp', comm_rtp1%nneib_domain
!      write(*,*) 'id_domain_rtp',    comm_rtp1%id_domain
!      write(*,*) 'ntot_item_sr_rtp', comm_rtp1%ntot_item_sr
!      write(*,*) 'istack_sr_rtp',    comm_rtp1%istack_sr
!
      call deallocate_domain_sr_tmp
      call alloc_type_sph_comm_item(nnod_rtp, comm_rtp1)
!
      icou = 0
      call set_comm_table_4_rtp(ip_rank, ndomain_sph, comm_rtm,         &
     &    comm_rtp1%ntot_item_sr, comm_rtp1%item_sr, icou)
!
      end subroutine const_comm_table_4_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rj(ip_rank, ndomain_sph, comm_rlm)
!
      use m_sph_trans_comm_table
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm(ndomain_sph)
!
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rlm(ip_org)%nneib_domain
          if(comm_rlm(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        nneib_domain_rj = nneib_domain_rj + 1
        id_domain_tmp(nneib_domain_rj) = id_org_rank
        nnod_sr_tmp(nneib_domain_rj)                                    &
     &     =  comm_rlm(ip_org)%istack_sr(iflag_jp)                      &
     &      - comm_rlm(ip_org)%istack_sr(iflag_jp-1)
      end do
!
      end subroutine count_comm_table_4_rj
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rj                                    &
     &         (ip_rank, ndomain_sph, comm_rlm, icou)
!
      use m_sph_trans_comm_table
!
      use t_spheric_rlm_data
!
      use set_local_index_table_sph
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm(ndomain_sph)
!
      integer(kind = kint), intent(inout) :: icou
!
      integer(kind = kint) :: jst, jed, j, jnod, k_tmp, j_tmp
      integer(kind = kint) :: k_glb, j_glb
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      call set_local_idx_table_rj
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rlm(ip_org)%nneib_domain
          if(comm_rlm(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        call const_sph_rlm_modes(id_org_rank)
!
        jst = comm_rlm1%istack_sr(iflag_jp-1)+1
        jed = comm_rlm1%istack_sr(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = comm_rlm1%item_sr(j)
          k_glb = sph_rlm1%idx_global_rlm(jnod,1)
          j_glb = sph_rlm1%idx_global_rlm(jnod,2)
          k_tmp = idx_local_rj_r(k_glb)
          j_tmp = idx_local_rj_j(j_glb)
          item_sr_rj(icou) =  j_tmp + (k_tmp-1) * nidx_rj(2)
        end do
!
        call dealloc_type_sph_comm_item(comm_rlm1)
        call dealloc_type_sph_1d_index_rlm(sph_rlm1)
        call dealloc_type_spheric_param_rlm(sph_rlm1)
      end do
!
      end subroutine set_comm_table_4_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_comm_table_4_rtp(ip_rank, ndomain_sph, comm_rtm, &
     &          nneib_domain_rtp)
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm(ndomain_sph)
!
      integer(kind = kint), intent(inout) :: nneib_domain_rtp
!
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org
      integer(kind = kint) :: iflag_jp
!
!
      nneib_domain_rtp = 0
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rtm(ip_org)%nneib_domain
          if(comm_rtm(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        nneib_domain_rtp = nneib_domain_rtp + 1
        id_domain_tmp(nneib_domain_rtp) = id_org_rank
        nnod_sr_tmp(nneib_domain_rtp)                                   &
     &     =  comm_rtm(ip_org)%istack_sr(iflag_jp)                      &
     &      - comm_rtm(ip_org)%istack_sr(iflag_jp-1)
      end do
!
      end subroutine count_comm_table_4_rtp
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_table_4_rtp                                   &
     &         (ip_rank, ndomain_sph, comm_rtm,                         &
     &          ntot_item_sr_rtp, item_sr_rtp, icou)
!
      use m_sph_trans_comm_table
      use set_local_index_table_sph
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ip_rank
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint), intent(in) :: ntot_item_sr_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtm(ndomain_sph)
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout)                               &
     &              :: item_sr_rtp(ntot_item_sr_rtp)
!
      integer(kind = kint) :: jst, jed, j, jnod
      integer(kind = kint) :: k_tmp, l_tmp, m_tmp, k_glb, l_glb, m_glb
      integer(kind = kint) :: ip1, jp, id_org_rank, ip_org, iflag_jp
!
!
      call set_local_idx_table_rtp
!
      do ip1 = 1, ndomain_sph
        id_org_rank = mod((ip_rank+ip1),ndomain_sph)
        ip_org = id_org_rank + 1
!
        iflag_jp = 0
        do jp = 1, comm_rtm(ip_org)%nneib_domain
          if(comm_rtm(ip_org)%id_domain(jp) .eq. ip_rank) then
            iflag_jp = jp
            exit
          end if
        end do
        if(iflag_jp .eq. 0) cycle
!
        call const_sph_rtm_grids(id_org_rank)
!
        jst = comm_rtm1%istack_sr(iflag_jp-1)+1
        jed = comm_rtm1%istack_sr(iflag_jp)
        do j =  jst, jed
          icou = icou + 1
          jnod = comm_rtm1%item_sr(j)
          k_glb = sph_rtm1%idx_global_rtm(jnod,1)
          l_glb = sph_rtm1%idx_global_rtm(jnod,2)
          m_glb = sph_rtm1%idx_global_rtm(jnod,3)
          k_tmp = idx_local_rtp_r(k_glb)
          l_tmp = idx_local_rtp_t(l_glb)
          m_tmp = idx_local_rtp_p(m_glb)
          item_sr_rtp(icou) =  k_tmp + (l_tmp-1) * nidx_rtp(1)          &
     &                        + (m_tmp-1) * nidx_rtp(1) * nidx_rtp(2)
        end do
!
        call dealloc_type_sph_comm_item(comm_rtm1)
        call dealloc_type_sph_1d_index_rtm(sph_rtm1)
        call dealloc_type_spheric_param_rtm(sph_rtm1)
      end do
!
      end subroutine set_comm_table_4_rtp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_comm_stack_rtp_rj(nneib_domain, id_domain,         &
     &          istack_sr, ntot_item_sr)
!
      integer(kind = kint), intent(in) :: nneib_domain
      integer(kind = kint), intent(inout) :: ntot_item_sr
      integer(kind = kint), intent(inout) :: id_domain(nneib_domain)
      integer(kind = kint), intent(inout) :: istack_sr(0:nneib_domain)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, nneib_domain
        id_domain(ip) = id_domain_tmp(ip)
        istack_sr(ip) = istack_sr(ip-1) + nnod_sr_tmp(ip)
      end do
      ntot_item_sr = istack_sr(nneib_domain)
!
      end subroutine set_comm_stack_rtp_rj
!
! -----------------------------------------------------------------------
!
      end module set_comm_table_rtp_rj
