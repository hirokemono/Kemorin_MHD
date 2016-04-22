!>@file   copy_sph_node_4_IO.f90
!!@brief  module copy_sph_node_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr indices from IO buffer
!!
!!@verbatim
!!      subroutine copy_sph_node_rtp_from_IO
!!      subroutine copy_sph_node_rtm_from_IO
!!      subroutine copy_sph_node_rlm_from_IO
!!      subroutine copy_sph_node_rj_from_IO
!!
!!      subroutine copy_sph_node_rtp_to_IO
!!      subroutine copy_sph_node_rtm_to_IO
!!      subroutine copy_sph_node_rlm_to_IO
!!      subroutine copy_sph_node_rj_to_IO
!!@endverbatim
!
      module copy_sph_node_4_IO
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_node_id_spherical_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtp_from_IO
!
      use t_spheric_rtp_data
!
      integer(kind = kint) :: i
!
      sph_rtp1%irank_sph_rtp(1:ithree) = sph_rank_IO(1:ithree)
!
      sph_rtp1%nidx_global_rtp(1:ithree) = nidx_gl_sph_IO(1:ithree)
      l_truncation =              ltr_gl_IO
!
      nnod_rtp =      nnod_sph_IO
!
      nidx_rtp(1:ithree) = nidx_sph_IO(1:ithree)
      sph_rtp1%ist_rtp(1:ithree) =  ist_sph_IO(1:ithree)
      sph_rtp1%ied_rtp(1:ithree) =  ied_sph_IO(1:ithree)
!
      sph_rtp1%nnod_rtp = nnod_rtp
      sph_rtp1%nidx_rtp(1:3) = nidx_rtp(1:3)
      call alloc_type_spheric_param_rtp(sph_rtp1)
      call alloc_type_sph_1d_index_rtp(sph_rtp1)
!
      do i = 1, ithree
        sph_rtp1%idx_global_rtp(1:nnod_rtp,i)                           &
     &      = idx_gl_sph_IO(1:nnod_rtp,i)
      end do
!
      sph_rtp1%radius_1d_rtp_r(1:nidx_rtp(1))                           &
     &      =   r_gl_1_IO(1:nidx_rtp(1))
      sph_rtp1%idx_gl_1d_rtp_r(1:nidx_rtp(1))                           &
     &      =   idx_gl_1_IO(1:nidx_rtp(1))
      sph_rtp1%idx_gl_1d_rtp_t(1:nidx_rtp(2))                           &
     &      = idx_gl_2_IO(1:nidx_rtp(2),1)
      sph_rtp1%idx_gl_1d_rtp_p(1:nidx_rtp(3),1)                         &
     &      = idx_gl_3_IO(1:nidx_rtp(3),1)
      sph_rtp1%idx_gl_1d_rtp_p(1:nidx_rtp(3),2)                         &
     &      = idx_gl_3_IO(1:nidx_rtp(3),2)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine copy_sph_node_rtp_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtm_from_IO
!
      use t_spheric_rtm_data
!
      integer(kind = kint) :: i
!
      sph_rtm1%irank_sph_rtm(1:ithree) =    sph_rank_IO(1:ithree)
!
      sph_rtm1%nidx_global_rtm(1:ithree) = nidx_gl_sph_IO(1:ithree)
      l_truncation =              ltr_gl_IO
!
      sph_rtm1%nnod_rtm = nnod_sph_IO
      sph_rtm1%nidx_rtm(1:ithree) = nidx_sph_IO(1:ithree)
      sph_rtm1%ist_rtm(1:ithree) =  ist_sph_IO(1:ithree)
      sph_rtm1%ied_rtm(1:ithree) =  ied_sph_IO(1:ithree)
!
      nnod_rtm = sph_rtm1%nnod_rtm
      nidx_rtm(1:3) = sph_rtm1%nidx_rtm(1:3)
      call alloc_type_spheric_param_rtm(sph_rtm1)
      call alloc_type_sph_1d_index_rtm(sph_rtm1)
!
      do i = 1, ithree
        sph_rtm1%idx_global_rtm(1:nnod_rtm,i)                           &
     &     = idx_gl_sph_IO(1:nnod_rtm,i)
      end do
!
      sph_rtm1%radius_1d_rtm_r(1:nidx_rtm(1))                           &
     &      =   r_gl_1_IO(1:nidx_rtm(1))
      sph_rtm1%idx_gl_1d_rtm_r(1:nidx_rtm(1))                           &
     &      =   idx_gl_1_IO(1:nidx_rtm(1))
      sph_rtm1%idx_gl_1d_rtm_t(1:nidx_rtm(2))                           &
     &      =   idx_gl_2_IO(1:nidx_rtm(2),1)
      sph_rtm1%idx_gl_1d_rtm_m(1:nidx_rtm(3),1)                         &
     &      = idx_gl_3_IO(1:nidx_rtm(3),1)
      sph_rtm1%idx_gl_1d_rtm_m(1:nidx_rtm(3),2)                         &
     &      = idx_gl_3_IO(1:nidx_rtm(3),2)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine copy_sph_node_rtm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rlm_from_IO
!
      integer(kind = kint) :: i
!
      sph_rlm1%irank_sph_rlm(1:itwo) =    sph_rank_IO(1:itwo)
!
      sph_rlm1%nidx_global_rlm(1:itwo) = nidx_gl_sph_IO(1:itwo)
      l_truncation =                     ltr_gl_IO
!
      sph_rlm1%nnod_rlm = nnod_sph_IO
      sph_rlm1%nidx_rlm(1:itwo) = nidx_sph_IO(1:itwo)
      sph_rlm1%ist_rlm(1:itwo) =  ist_sph_IO(1:itwo)
      sph_rlm1%ied_rlm(1:itwo) =  ied_sph_IO(1:itwo)
!
      nnod_rlm = sph_rlm1%nnod_rlm
      nidx_rlm(1:2) = sph_rlm1%nidx_rlm(1:2)
      call alloc_type_spheric_param_rlm(sph_rlm1)
      call alloc_type_sph_1d_index_rlm(sph_rlm1)
!
      do i = 1, itwo
        sph_rlm1%idx_global_rlm(1:nnod_rlm,i)                           &
     &        = idx_gl_sph_IO(1:nnod_rlm,i)
      end do
!
      sph_rlm1%radius_1d_rlm_r(1:nidx_rlm(1))                           &
     &                = r_gl_1_IO(1:nidx_rlm(1))
      sph_rlm1%idx_gl_1d_rlm_r(1:nidx_rlm(1))                           &
     &                = idx_gl_1_IO(1:nidx_rlm(1))
      sph_rlm1%idx_gl_1d_rlm_j(1:nidx_rlm(2),1)                         &
     &                = idx_gl_2_IO(1:nidx_rlm(2),1)
      sph_rlm1%idx_gl_1d_rlm_j(1:nidx_rlm(2),2)                         &
     &                = idx_gl_2_IO(1:nidx_rlm(2),2)
      sph_rlm1%idx_gl_1d_rlm_j(1:nidx_rlm(2),3)                         &
     &                = idx_gl_2_IO(1:nidx_rlm(2),3)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine copy_sph_node_rlm_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rj_from_IO
!
      use t_spheric_rj_data
!
      integer(kind = kint) :: i
!
      sph_rj1%irank_sph_rj(1:itwo) =     sph_rank_IO(1:itwo)
!
      sph_rj1%nidx_global_rj(1:itwo) = nidx_gl_sph_IO(1:itwo)
      l_truncation =            ltr_gl_IO
!
      sph_rj1%nnod_rj = nnod_sph_IO
      sph_rj1%nidx_rj(1:itwo) = nidx_sph_IO(1:itwo)
      sph_rj1%ist_rj(1:itwo) =  ist_sph_IO(1:itwo)
      sph_rj1%ied_rj(1:itwo) =  ied_sph_IO(1:itwo)
!
      call alloc_type_spheric_param_rj(sph_rj1)
      call alloc_type_sph_1d_index_rj(sph_rj1)
!
      do i = 1, itwo
        sph_rj1%idx_global_rj(1:sph_rj1%nnod_rj,i)                      &
     &        = idx_gl_sph_IO(1:sph_rj1%nnod_rj,i)
      end do
!
      sph_rj1%radius_1d_rj_r(1:sph_rj1%nidx_rj(1))                      &
     &      =   r_gl_1_IO(1:sph_rj1%nidx_rj(1))
      sph_rj1%a_r_1d_rj_r(1:sph_rj1%nidx_rj(1))                         &
     &      = one / sph_rj1%radius_1d_rj_r(1:sph_rj1%nidx_rj(1))
!
      sph_rj1%idx_gl_1d_rj_r(1:sph_rj1%nidx_rj(1))                      &
     &       =   idx_gl_1_IO(1:sph_rj1%nidx_rj(1))
      sph_rj1%idx_gl_1d_rj_j(1:sph_rj1%nidx_rj(2),1)                    &
     &       = idx_gl_2_IO(1:sph_rj1%nidx_rj(2),1)
      sph_rj1%idx_gl_1d_rj_j(1:sph_rj1%nidx_rj(2),2)                    &
     &       = idx_gl_2_IO(1:sph_rj1%nidx_rj(2),2)
      sph_rj1%idx_gl_1d_rj_j(1:sph_rj1%nidx_rj(2),3)                    &
     &       = idx_gl_2_IO(1:sph_rj1%nidx_rj(2),3)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      nnod_rj = sph_rj1%nnod_rj
      nidx_rj(1:2) = sph_rj1%nidx_rj(1:2)
!
      end subroutine copy_sph_node_rj_from_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtp_to_IO
!
      use t_spheric_rtp_data
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8, nrt8
!
!
      ndir_sph_IO =           ithree
      sph_rank_IO(1:ithree) = sph_rtp1%irank_sph_rtp(1:ithree)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ione
      ncomp_itbl_1d_IO(3) = itwo
!
      nidx_gl_sph_IO(1:ithree) = sph_rtp1%nidx_global_rtp(1:ithree)
      ltr_gl_IO =                l_truncation
!
      nnod_sph_IO = nnod_rtp
      nidx_sph_IO(1:ithree) = nidx_rtp(1:ithree)
      ist_sph_IO(1:ithree) =  sph_rtp1%ist_rtp(1:ithree)
      ied_sph_IO(1:ithree) =  sph_rtp1%ied_rtp(1:ithree)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
!$omp parallel do private(i,nr_8,nrt8)
      do i = 1, nnod_rtp
        nr_8 = sph_rtp1%nidx_global_rtp(1)
        nrt8 = sph_rtp1%nidx_global_rtp(1)*sph_rtp1%nidx_global_rtp(2)
        idx_gl_sph_IO(i,1) = sph_rtp1%idx_global_rtp(i,1)
        idx_gl_sph_IO(i,2) = sph_rtp1%idx_global_rtp(i,2)
        idx_gl_sph_IO(i,3) = sph_rtp1%idx_global_rtp(i,3)
        inod_gl_sph_IO(i) = sph_rtp1%idx_global_rtp(i,1)                &
     &                   + (sph_rtp1%idx_global_rtp(i,2) - 1) * nr_8    &
     &                   + (sph_rtp1%idx_global_rtp(i,3) - 1) * nrt8
      end do
!$omp end parallel do
!
      r_gl_1_IO(1:nidx_rtp(1))                                          &
     &      = sph_rtp1%radius_1d_rtp_r(1:nidx_rtp(1))
      idx_gl_1_IO(1:nidx_rtp(1))                                        &
     &      = sph_rtp1%idx_gl_1d_rtp_r(1:nidx_rtp(1))
      idx_gl_2_IO(1:nidx_rtp(2),1)                                      &
     &      = sph_rtp1%idx_gl_1d_rtp_t(1:nidx_rtp(2))
      idx_gl_3_IO(1:nidx_rtp(3),1)                                      &
     &      = sph_rtp1%idx_gl_1d_rtp_p(1:nidx_rtp(3),1)
      idx_gl_3_IO(1:nidx_rtp(3),2)                                      &
     &      = sph_rtp1%idx_gl_1d_rtp_p(1:nidx_rtp(3),2)
!
      call dealloc_type_sph_1d_index_rtp(sph_rtp1)
      call dealloc_type_spheric_param_rtp(sph_rtp1)
!
      end subroutine copy_sph_node_rtp_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rtm_to_IO
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8, nrt8
!
!
      ndir_sph_IO =              ithree
      sph_rank_IO(1:ithree) =    sph_rtm1%irank_sph_rtm(1:ithree)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ione
      ncomp_itbl_1d_IO(3) = itwo
!
      nidx_gl_sph_IO(1:ithree) = sph_rtm1%nidx_global_rtm(1:ithree)
      ltr_gl_IO =                l_truncation
!
      nnod_sph_IO = nnod_rtm
      nidx_sph_IO(1:ithree) = nidx_rtm(1:ithree)
      ist_sph_IO(1:ithree) =  sph_rtm1%ist_rtm(1:ithree)
      ied_sph_IO(1:ithree) =  sph_rtm1%ied_rtm(1:ithree)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
!$omp parallel do private(i,nr_8,nrt8)
      do i = 1, nnod_rtm
        nr_8 = sph_rtm1%nidx_global_rtm(1)
        nrt8 = sph_rtm1%nidx_global_rtm(1)*sph_rtm1%nidx_global_rtm(2)
        idx_gl_sph_IO(i,1) = sph_rtm1%idx_global_rtm(i,1)
        idx_gl_sph_IO(i,2) = sph_rtm1%idx_global_rtm(i,2)
        idx_gl_sph_IO(i,3) = sph_rtm1%idx_global_rtm(i,3)
        inod_gl_sph_IO(i) = sph_rtm1%idx_global_rtm(i,1)                &
     &                   + (sph_rtm1%idx_global_rtm(i,2) - 1) * nr_8    &
     &                   +  sph_rtm1%idx_global_rtm(i,3) * nrt8
      end do
!$omp end parallel do
!
      r_gl_1_IO(1:nidx_rtm(1))                                          &
     &      = sph_rtm1%radius_1d_rtm_r(1:nidx_rtm(1))
      idx_gl_1_IO(1:nidx_rtm(1))                                        &
     &      =   sph_rtm1%idx_gl_1d_rtm_r(1:nidx_rtm(1))
      idx_gl_2_IO(1:nidx_rtm(2),1)                                      &
     &      = sph_rtm1%idx_gl_1d_rtm_t(1:nidx_rtm(2))
      idx_gl_3_IO(1:nidx_rtm(3),1)                                      &
     &      = sph_rtm1%idx_gl_1d_rtm_m(1:nidx_rtm(3),1)
      idx_gl_3_IO(1:nidx_rtm(3),2)                                      &
     &      = sph_rtm1%idx_gl_1d_rtm_m(1:nidx_rtm(3),2)
!
      call dealloc_type_sph_1d_index_rtm(sph_rtm1)
      call dealloc_type_spheric_param_rtm(sph_rtm1)
!
      end subroutine copy_sph_node_rtm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rlm_to_IO
!
      use t_spheric_rlm_data
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8
!
!
      ndir_sph_IO =            itwo
      sph_rank_IO(1:itwo) =    sph_rlm1%irank_sph_rlm(1:itwo)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ithree
!
      nidx_gl_sph_IO(1:itwo) = sph_rlm1%nidx_global_rlm(1:itwo)
      ltr_gl_IO =              l_truncation
!
      nnod_sph_IO = nnod_rlm
      nidx_sph_IO(1:itwo) = nidx_rlm(1:itwo)
      ist_sph_IO(1:itwo) =  sph_rlm1%ist_rlm(1:itwo)
      ied_sph_IO(1:itwo) =  sph_rlm1%ied_rlm(1:itwo)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
!$omp parallel do private(i,nr_8)
      do i = 1, nnod_rlm
        nr_8 = sph_rlm1%nidx_global_rlm(1)
        idx_gl_sph_IO(i,1) = sph_rlm1%idx_global_rlm(i,1)
        idx_gl_sph_IO(i,2) = sph_rlm1%idx_global_rlm(i,2)
        inod_gl_sph_IO(i) =  sph_rlm1%idx_global_rlm(i,1)               &
     &                     + sph_rlm1%idx_global_rlm(i,2) * nr_8
      end do
!$omp end parallel do
      do i = 1, itwo
        idx_gl_sph_IO(1:nnod_rlm,i)                                     &
     &      = sph_rlm1%idx_global_rlm(1:nnod_rlm,i)
      end do
      inod_gl_sph_IO(1:nnod_rlm)                                        &
     &           =  sph_rlm1%idx_global_rlm(1:nnod_rlm,1)               &
     &            + sph_rlm1%idx_global_rlm(1:nnod_rlm,2)               &
     &             * sph_rlm1%nidx_global_rlm(1)
!
      r_gl_1_IO(1:nidx_rlm(1))                                          &
     &           = sph_rlm1%radius_1d_rlm_r(1:nidx_rlm(1))
      idx_gl_1_IO(1:nidx_rlm(1))                                        &
     &           = sph_rlm1%idx_gl_1d_rlm_r(1:nidx_rlm(1))
      idx_gl_2_IO(1:nidx_rlm(2),1)                                      &
     &           = sph_rlm1%idx_gl_1d_rlm_j(1:nidx_rlm(2),1)
      idx_gl_2_IO(1:nidx_rlm(2),2)                                      &
     &           = sph_rlm1%idx_gl_1d_rlm_j(1:nidx_rlm(2),2)
      idx_gl_2_IO(1:nidx_rlm(2),3)                                      &
     &           = sph_rlm1%idx_gl_1d_rlm_j(1:nidx_rlm(2),3)
!
      call dealloc_type_sph_1d_index_rlm(sph_rlm1)
      call dealloc_type_spheric_param_rlm(sph_rlm1)
!
      end subroutine copy_sph_node_rlm_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine copy_sph_node_rj_to_IO
!
      use t_spheric_rj_data
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: nr_8
!
      ndir_sph_IO =            itwo
      sph_rank_IO(1:itwo) =    sph_rj1%irank_sph_rj(1:itwo)
!
      ncomp_itbl_1d_IO(1) = ione
      ncomp_itbl_1d_IO(2) = ithree
!
      nidx_gl_sph_IO(1:itwo) = sph_rj1%nidx_global_rj(1:itwo)
      ltr_gl_IO =              l_truncation
!
      nnod_sph_IO = sph_rj1%nnod_rj
      nidx_sph_IO(1:itwo) = sph_rj1%nidx_rj(1:itwo)
      ist_sph_IO(1:itwo) =  sph_rj1%ist_rj(1:itwo)
      ied_sph_IO(1:itwo) =  sph_rj1%ied_rj(1:itwo)
!
      call allocate_nod_id_sph_IO
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
!$omp parallel do private(i,nr_8)
      do i = 1, sph_rj1%nnod_rj
        nr_8 = sph_rj1%nidx_global_rj(1)
        idx_gl_sph_IO(i,1) = sph_rj1%idx_global_rj(i,1)
        idx_gl_sph_IO(i,2) = sph_rj1%idx_global_rj(i,2)
        inod_gl_sph_IO(i) =  sph_rj1%idx_global_rj(i,1)                 &
     &                     + sph_rj1%idx_global_rj(i,2) * nr_8
      end do
!$omp end parallel do
!
      if(inod_gl_sph_IO(sph_rj1%nnod_rj) .eq. izero) then
        nr_8 = (sph_rj1%nidx_global_rj(2) + 1)
        inod_gl_sph_IO(sph_rj1%nnod_rj)                                 &
     &          = sph_rj1%nidx_global_rj(1) * nr_8 + 1
      end if
!
      r_gl_1_IO(1:sph_rj1%nidx_rj(1))                                   &
     &      = sph_rj1%radius_1d_rj_r(1:sph_rj1%nidx_rj(1))
      idx_gl_1_IO(1:sph_rj1%nidx_rj(1))                                 &
     &      = sph_rj1%idx_gl_1d_rj_r(1:sph_rj1%nidx_rj(1))
      idx_gl_2_IO(1:sph_rj1%nidx_rj(2),1)                               &
     &      = sph_rj1%idx_gl_1d_rj_j(1:sph_rj1%nidx_rj(2),1)
      idx_gl_2_IO(1:sph_rj1%nidx_rj(2),2)                               &
     &      = sph_rj1%idx_gl_1d_rj_j(1:sph_rj1%nidx_rj(2),2)
      idx_gl_2_IO(1:sph_rj1%nidx_rj(2),3)                               &
     &      = sph_rj1%idx_gl_1d_rj_j(1:sph_rj1%nidx_rj(2),3)
!
      call dealloc_type_sph_1d_index_rj(sph_rj1)
      call dealloc_spheric_param_rj(sph_rj1)
!
      end subroutine copy_sph_node_rj_to_IO
!
! ----------------------------------------------------------------------
!
      end module copy_sph_node_4_IO
