!>@file   copy_sph_comm_table_4_IO.f90
!!@brief  module copy_sph_comm_table_4_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!>@brief  Copy communication table for spherical hermonica transform
!!        between IO data
!!
!!@verbatim
!!      subroutine copy_comm_rtp_from_IO(nnod_rtp)
!!      subroutine copy_comm_rtm_from_IO(nnod_rtm)
!!      subroutine copy_comm_rlm_from_IO(nnod_rlm)
!!      subroutine copy_comm_rj_from_IO(nnod_rj)
!!
!!      subroutine copy_comm_rtp_to_IO(my_rank)
!!      subroutine copy_comm_rtm_to_IO(my_rank)
!!      subroutine copy_comm_rlm_to_IO(my_rank)
!!      subroutine copy_comm_rj_to_IO(my_rank)
!!@endverbatim
!!
!!@n @param my_rank   running process ID
!!@n @param nnod_rtp
!!      number of data points for @f$ f(r,\theta,\phi) @f$
!!@n @param nnod_rtm
!!      number of data points for @f$ f(r,\theta,m) @f$
!!@n @param nnod_rlm 
!!      number of data points for @f$ f(r,l,m) @f$
!!@n @param nnod_rj  
!!      number of data points for @f$ f(r,j) @f$
!!
!
      module copy_sph_comm_table_4_IO
!
      use m_precision
!
      use m_constants
      use m_sph_trans_comm_table
      use m_comm_data_IO
      use copy_sph_comm_tbl_type_4_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_from_IO(nnod_rtp, comm_rtp)
!
      use copy_sph_comm_tbl_type_4_IO
!
      integer(kind = kint), intent(in) :: nnod_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtp
!
!
      comm_rtp%nneib_domain = num_neib_domain_IO
      comm_rtp%ntot_item_sr = ntot_import_IO
!
      call alloc_type_sph_comm_stack(comm_rtp)
      call alloc_type_sph_comm_item(nnod_rtp, comm_rtp)
!
      comm_rtp%id_domain(1:comm_rtp%nneib_domain)                       &
     &      = id_neib_domain_IO(1:comm_rtp%nneib_domain)
      comm_rtp%istack_sr(0:comm_rtp%nneib_domain)                       &
     &      = istack_import_IO(0:comm_rtp%nneib_domain)
!
      comm_rtp%item_sr(1:comm_rtp%ntot_item_sr)                         &
     &      = item_import_IO(1:comm_rtp%ntot_item_sr)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rtp_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_from_IO(nnod_rtm)
!
      integer(kind = kint), intent(in) :: nnod_rtm
!
!
      comm_rtm1%nneib_domain = num_neib_domain_IO
      comm_rtm1%ntot_item_sr = ntot_import_IO
!
      call alloc_type_sph_comm_stack(comm_rtm1)
      call alloc_type_sph_comm_item(nnod_rtm, comm_rtm1)
!
      comm_rtm1%id_domain(1:comm_rtm1%nneib_domain)                     &
     &      = id_neib_domain_IO(1:comm_rtm1%nneib_domain)
      comm_rtm1%istack_sr(0:comm_rtm1%nneib_domain)                     &
     &      = istack_import_IO(0:comm_rtm1%nneib_domain)
!
      comm_rtm1%item_sr(1:comm_rtm1%ntot_item_sr)                       &
     &      = item_import_IO(1:comm_rtm1%ntot_item_sr)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rtm_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_from_IO(nnod_rlm)
!
      integer(kind = kint), intent(in) :: nnod_rlm
!
!
      nneib_domain_rlm = num_neib_domain_IO
      ntot_item_sr_rlm = ntot_import_IO
!
      call allocate_sph_comm_stack_rlm
      call allocate_sph_comm_item_rlm(nnod_rlm)
!
      id_domain_rlm(1:nneib_domain_rlm)                                 &
     &      = id_neib_domain_IO(1:nneib_domain_rlm)
      istack_sr_rlm(0:nneib_domain_rlm)                                 &
     &      = istack_import_IO(0:nneib_domain_rlm)
!
      item_sr_rlm(1:ntot_item_sr_rlm)                                   &
     &      = item_import_IO(1:ntot_item_sr_rlm)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rlm_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_from_IO(nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      nneib_domain_rj = num_neib_domain_IO
      ntot_item_sr_rj = ntot_import_IO
!
      call allocate_sph_comm_stack_rj
      call allocate_sph_comm_item_rj(nnod_rj)
!
      id_domain_rj(1:nneib_domain_rj)                                   &
     &      = id_neib_domain_IO(1:nneib_domain_rj)
      istack_sr_rj(0:nneib_domain_rj)                                   &
     &      = istack_import_IO(0:nneib_domain_rj)
!
      item_sr_rj(1:ntot_item_sr_rj)                                     &
     &      = item_import_IO(1:ntot_item_sr_rj)
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine copy_comm_rj_from_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtp_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      call copy_comm_sph_type_to_IO(my_rank, comm_rtp1)
!
      end subroutine copy_comm_rtp_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rtm_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
      num_neib_domain_IO = comm_rtm1%nneib_domain
      ntot_import_IO =     comm_rtm1%ntot_item_sr
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:comm_rtm1%nneib_domain)                       &
     &      = comm_rtm1%id_domain(1:comm_rtm1%nneib_domain)
      istack_import_IO(0:comm_rtm1%nneib_domain)                        &
     &      = comm_rtm1%istack_sr(0:comm_rtm1%nneib_domain)
!
      item_import_IO(1:comm_rtm1%ntot_item_sr)                          &
     &      = comm_rtm1%item_sr(1:comm_rtm1%ntot_item_sr)
!
      call dealloc_type_sph_comm_item(comm_rtm1)
!
      end subroutine copy_comm_rtm_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rlm_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
      num_neib_domain_IO = nneib_domain_rlm
      ntot_import_IO =     ntot_item_sr_rlm
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:nneib_domain_rlm)                             &
     &      = id_domain_rlm(1:nneib_domain_rlm)
      istack_import_IO(0:nneib_domain_rlm)                              &
     &      = istack_sr_rlm(0:nneib_domain_rlm)
!
      item_import_IO(1:ntot_item_sr_rlm)                                &
     &      = item_sr_rlm(1:ntot_item_sr_rlm)
!
      call deallocate_sph_comm_item_rlm
!
      end subroutine copy_comm_rlm_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_comm_rj_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
      num_neib_domain_IO = nneib_domain_rj
      ntot_import_IO =     ntot_item_sr_rj
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_import_item_IO
!
      id_neib_domain_IO(1:nneib_domain_rj)                              &
     &      = id_domain_rj(1:nneib_domain_rj)
      istack_import_IO(0:nneib_domain_rj)                               &
     &      = istack_sr_rj(0:nneib_domain_rj)
!
      item_import_IO(1:ntot_item_sr_rj)                                 &
     &      = item_sr_rj(1:ntot_item_sr_rj)
!
      call deallocate_sph_comm_item_rj
!
      end subroutine copy_comm_rj_to_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_comm_table_4_IO
