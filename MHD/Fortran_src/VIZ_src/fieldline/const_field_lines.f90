!>@file   const_field_lines.f90
!!@brief  module const_field_lines
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_const_field_lines                                  &
!!     &         (node, ele, surf, ele_4_nod, nod_comm, nod_fld,        &
!!     &          fln_prm, fln_src, fln_tce, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(local_fieldline), intent(inout) :: fline_lc
!!@endverbatim
!
      module const_field_lines
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_para_double_numbering
!
      implicit  none
!
      private :: recover_local_fline_start
      private :: set_fline_start_2_bcast, set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_field_lines(node, ele, surf, ele_4_nod,        &
     &          nod_comm, inod_dbl, iele_dbl, isurf_dbl, isf_4_ele_dbl, iele_4_surf_dbl,          &
     &          nod_fld, fln_prm, fln_src, fln_tce, fline_lc)
!
      use t_control_params_4_fline
      use t_comm_table
      use t_next_node_ele_4_node
      use t_phys_data
      use t_local_fline
      use t_source_of_filed_line
      use calypso_mpi_real
      use calypso_mpi_int
      use transfer_to_long_integers
      use extend_field_line
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(in) :: ele_4_nod
      type(communication_table), intent(in) :: nod_comm
      type(node_ele_double_number), intent(in) :: inod_dbl, iele_dbl
      type(node_ele_double_number), intent(in) :: isurf_dbl
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in)                               &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: iflag_comm
      integer(kind = kint) :: i, ist, ied, ip, nline, inum
      integer(kind = kint_gl) :: num64
      integer :: src_rank
!
!
      if(i_debug .gt. iflag_full_msg) then
        write(my_rank+50,*)                                             &
     &         'num_current_fline', fln_tce%num_current_fline(:)
        write(my_rank+50,*)                                             &
     &         'istack_current_fline', fln_tce%istack_current_fline(:)
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        write(my_rank+50,*) 'isf_fline_start(1:3,inum)'
        do inum = ist, ied
          write(my_rank+50,*) inum, fln_tce%isf_fline_start(1:3,inum)
        end do
      end if
      call calypso_MPI_barrier
!
      iflag_comm = 0
      call reset_fline_start(fline_lc)
!
      do
        ist = fln_tce%istack_current_fline(my_rank) + 1
        ied = fln_tce%istack_current_fline(my_rank+1)
        write(*,*) 'fln_tce%istack_current_fline', my_rank,             &
     &            fln_tce%istack_current_fline(my_rank:my_rank+1),      &
     &            fln_tce%num_current_fline(my_rank+1)
        do inum = ist, ied
          call s_extend_field_line(node, ele, surf, nod_fld, fln_prm,   &
     &        fln_prm%max_line_stepping, fln_prm%max_trace_length,      &
     &        fln_prm%iflag_fline_used_ele,                             &
     &        fln_tce%iflag_fline(inum), fln_src%vector_nod_fline,      &
     &        fln_tce%isf_fline_start(1,inum),                          &
     &        fln_tce%xx_fline_start(1,inum),                           &
     &        fln_tce%v_fline_start(1,inum),                            &
     &        fln_tce%c_fline_start(1,inum),                            &
     &        fln_tce%icount_fline(inum), fln_tce%trace_length(inum),   &
     &        iflag_comm, fline_lc)
          write(50+my_rank,*) 'extension end for ', inum, iflag_comm
!
          call set_fline_start_2_bcast                                  &
     &       (iflag_comm, inum, ele, surf, inod_dbl, iele_dbl, isurf_dbl, &
     &        isf_4_ele_dbl, iele_4_surf_dbl, fln_prm%ntot_color_comp,                 &
     &          nod_comm%num_neib, nod_comm%id_neib,                    &
     &          nod_comm%ntot_import,  nod_comm%istack_import,          &
     &          nod_comm%item_import, fln_tce)
        end do
        call calypso_MPI_barrier
!
        do ip = 1, nprocs
          src_rank = int(ip - 1)
          ist = fln_tce%istack_current_fline(ip-1)
          num64 = fln_tce%num_current_fline(ip)
          if(num64 .le. 0) cycle
            call calypso_mpi_bcast_int                                  &
     &         (fln_tce%id_fline_export(1,ist+1),                       &
     &          (num64*fln_tce%nitem_export), src_rank)
            call calypso_mpi_bcast_real(fln_tce%fline_export(1,ist+1),  &
     &          (num64*fln_tce%ncomp_export), src_rank)
        end do
!
        if(iflag_debug .gt. 0) then
          write(my_rank+50,*) 'i, new_start_pe, id_fline_export'
          do i = 1, fln_tce%istack_current_fline(nprocs)
            write(my_rank+50,'(10i16)')                                 &
     &              i, fln_tce%id_fline_export(1:3,i)
          end do
        end if
!
        call recover_local_fline_start(ele, surf, inod_dbl, iele_dbl,        &
     &      node%numnod, isf_4_ele_dbl, iele_4_surf_dbl, ele_4_nod%ntot,           &
     &      ele_4_nod%istack_4_node, ele_4_nod%iele_4_node,             &
     &      nod_comm%num_neib, nod_comm%id_neib, nod_comm%ntot_export,  &
     &      nod_comm%istack_export, nod_comm%item_export, fln_tce)
        call set_fline_start_from_neib(fln_prm, fln_tce)
!
        nline = fln_tce%istack_current_fline(nprocs)                    &
     &         - fln_tce%istack_current_fline(0)
        if(i_debug .gt. 0) then
          write(my_rank+50,*) 'istack_current_fline',                   &
     &                       fln_tce%istack_current_fline(:)
!
          write(my_rank+50,*) 'number of lines: ', nline
          write(*,*) 'number of lines: ', my_rank, nline
        end if
       if(nline .le. 0) exit
      end do
!
!      call check_local_fline_dx( (my_rank+60), fline_lc)
!
      end subroutine s_const_field_lines
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_2_bcast(iflag_comm, i,                 &
     &          ele, surf, inod_dbl, iele_dbl, isurf_dbl,  &
     &    isf_4_ele_dbl, iele_4_surf_dbl, ntot_comp,   &
     &          num_neib, id_neib, ntot_import, istack_import,          &
     &          item_import, fln_tce)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: iflag_comm, i
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(node_ele_double_number), intent(in) :: inod_dbl, iele_dbl
      type(node_ele_double_number), intent(in) :: isurf_dbl
      integer(kind = kint), intent(in)                               &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
      integer(kind = kint), intent(in) :: ntot_comp
!
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: item_import(ntot_import)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: inod, iele, isf, ip, ist, ied, inum
      integer(kind = kint) :: isurf
!
!
      if(iflag_comm .eq. ione) then
        iele = fln_tce%isf_fline_start(1,i)
        isf =  fln_tce%isf_fline_start(2,i)
        inod = fln_tce%isf_fline_start(3,i)
        do ip = 1, num_neib
          ist = istack_import(ip-1) + 1
          ied = istack_import(ip)
          do inum = ist, ied
            if(item_import(inum) .eq. inod) then
              fln_tce%id_fline_export(1,i) = id_neib(ip)
              fln_tce%id_fline_export(7,i) = 1 + inum - ist
              exit
            end if
          end do
        end do
        isurf = abs(surf%isf_4_ele(iele,isf))
!        if(surf%isf_4_ele(iele,isf) .lt. 0) then
          write(*,*)  my_rank, iele, isf, surf%isf_4_ele(iele,isf),&
     &      'iele_4_surf_dbl_1', &
     &          surf%iele_4_surf(isurf, 1, 1:2), ':  ', &
     &          iele_4_surf_dbl(isurf, 1, 1:3),  &
     &          isf_4_ele_dbl(iele,isf,1:2)

!        else
          write(*,*)  my_rank, iele, isf, surf%isf_4_ele(iele,isf),&
     &       'iele_4_surf_dbl_2', &
     &          surf%iele_4_surf(isurf, 2, 1:2), ':  ', &
     &          iele_4_surf_dbl(isurf, 2, 1:3), &
     &          isurf_dbl%irank(isurf), isurf_dbl%index(isurf)
!        end if
!
        fln_tce%id_fline_export(2,i) = fln_tce%iflag_fline(i)
        fln_tce%id_fline_export(3,i) = fln_tce%icount_fline(i)
        fln_tce%id_fline_export(5,i) = isf
        fln_tce%id_fline_export(6,i) = surf%isf_4_ele(iele,isf)
!
        fln_tce%id_fline_export( 8,i) = int(iele_dbl%irank(iele))
        fln_tce%id_fline_export( 9,i) = int(iele_dbl%index(iele))
        fln_tce%id_fline_export(10,i) = int(inod_dbl%irank(inod))
        fln_tce%id_fline_export(11,i) = int(inod_dbl%index(inod))
!
        fln_tce%id_fline_export(12:14,i) = iele_4_surf_dbl(isurf, 1, 1:3)
        fln_tce%id_fline_export(15:17,i) = iele_4_surf_dbl(isurf, 2, 1:3)
        fln_tce%id_fline_export(18:19,i) = isf_4_ele_dbl(iele,isf,1:2)
!
        fln_tce%fline_export(1:4,i) = fln_tce%xx_fline_start(1:4,i)
        fln_tce%fline_export(5:8,i) = fln_tce%v_fline_start(1:4,i)
        fln_tce%fline_export(9,i) =   fln_tce%trace_length(i)
        fln_tce%fline_export(9+1:9+ntot_comp,i)                         &
    &         = fln_tce%c_fline_start(1:ntot_comp,i)
      else
        fln_tce%id_fline_export(1,i) =   -ione
        fln_tce%id_fline_export(2:11,i) = izero
        fln_tce%fline_export(1:fln_tce%ncomp_export,i) = zero
      end if
!
      end subroutine set_fline_start_2_bcast
!
!  ---------------------------------------------------------------------
!
      subroutine recover_local_fline_start(ele, surf, inod_dbl, iele_dbl,    &
     &          numnod, isf_4_ele_dbl, iele_4_surf_dbl, ntot_ele_4_node,               &
     &          iele_stack_4_node, iele_4_node, num_neib, id_neib,      &
     &          ntot_export, istack_export, item_export, fln_tce)
!
      use m_geometry_constants
      use t_source_of_filed_line
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(node_ele_double_number), intent(in) :: inod_dbl, iele_dbl
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in)                               &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer (kind=kint), intent(in) :: iele_4_node(ntot_ele_4_node)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_export
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in) :: item_export(ntot_export)
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ip, ip_org, ist_lin, ied_lin, i
      integer(kind = kint) :: inum, inod, ist_ele, ied_ele, jnum, jele
      integer(kind = kint) :: isf, isurf, is(2)
!
!
      do ip = 1, num_neib
        ip_org = id_neib(ip) + 1
!
        ist_lin = fln_tce%istack_current_fline(ip_org-1) + 1
        ied_lin = fln_tce%istack_current_fline(ip_org)
        do i = ist_lin, ied_lin
          if(fln_tce%id_fline_export(1,i) .eq. my_rank) then
          
            if(fln_tce%id_fline_export(19,i) .lt. 0) then
              is(1:2) = fln_tce%id_fline_export(13:14,i)
            else
              is(1:2) = fln_tce%id_fline_export(16:17,i)
            end if
          
          
            inum = fln_tce%id_fline_export(7,i) + istack_export(ip-1)
            inod = item_export(inum)
!            write(60+my_rank,*) 'recover node', inod,                  &
!     &           inod_global(inod),fln_tce%id_fline_export(6,i)
!
            ist_ele = iele_stack_4_node(inod-1) + 1
            ied_ele = iele_stack_4_node(inod)
            do jnum = ist_ele, ied_ele
              isf =  fln_tce%id_fline_export(5,i)
              jele = iele_4_node(jnum)
              if(fln_tce%id_fline_export(8,i) .eq. iele_dbl%irank(jele) &
               .and. fln_tce%id_fline_export(9,i)                       &
      &                    .eq. iele_dbl%index(jele)) then
                isurf = abs(surf%isf_4_ele(jele,isf))
!
                 write(*,*) i, my_rank, jele, &
      &                 iele_dbl%index(jele), 'jele_comp', &
      &                 fln_tce%id_fline_export(18:19,i), &
      &                 fln_tce%id_fline_export(6,i), &
      &                  fln_tce%id_fline_export(13,i), &
      &                  fln_tce%id_fline_export(16,i), &
      &                  fln_tce%id_fline_export(12,i), &
      &               surf%isf_4_ele(fln_tce%id_fline_export(13,i),isf),&
      &                  fln_tce%id_fline_export(15,i), &
      &               surf%isf_4_ele(fln_tce%id_fline_export(16,i),isf) &
      &
                 
!
                if(surf%isf_4_ele(jele,isf) .lt. 0) then
!                if(fln_tce%id_fline_export(6,i) .lt. 0) then
                  fln_tce%id_fline_export(4,i) = surf%iele_4_surf(isurf,1,1)
                  fln_tce%id_fline_export(5,i) = surf%iele_4_surf(isurf,1,2)
!              write(*,*)  i, my_rank, jele, isf,  &
!     &         surf%isf_4_ele(jele,isf), &
!     &          fln_tce%id_fline_export(6,i), 'found', &
!     &          fln_tce%id_fline_export(3,i), 'found', &
!     &          surf%iele_4_surf(isurf, 1, 1:2), ':  ', &
!     &          iele_4_surf_dbl(isurf, 1, 1:3),   ':  ', &
!     &          fln_tce%id_fline_export(12:17,i)
                else
                  fln_tce%id_fline_export(4,i) = surf%iele_4_surf(isurf,2,1)
                  fln_tce%id_fline_export(5,i) = surf%iele_4_surf(isurf,2,2)
!              write(*,*)  i, my_rank, jele, isf,&
!     & surf%isf_4_ele(jele,isf), &
!     &          fln_tce%id_fline_export(6,i), 'found', &
!     &          fln_tce%id_fline_export(3,i), 'found', &
!     &          surf%iele_4_surf(isurf, 2, 1:2), ':  ', &
!     &          iele_4_surf_dbl(isurf, 2, 1:3),   ':  ', &
!     &          fln_tce%id_fline_export(12:17,i)
                end if
              write(*,*)  'Found:   ', fln_tce%id_fline_export(4:5,i), &
          &   is
!                write(60+my_rank,*) 'recover surf',                    &
!     &                         fln_tce%id_fline_export(4:5,i)
!
                exit
              end if
            end do
            fln_tce%id_fline_export(6,i) = inod
!
          end if
        end do
      end do
      call calypso_mpi_barrier
!
      end subroutine recover_local_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_from_neib(fln_prm, fln_tce)
!
      use calypso_mpi_int
      use t_source_of_filed_line
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ied_lin, i, icou, ip
!
!
      ied_lin = fln_tce%istack_current_fline(nprocs)
      icou = 0
      do i = 1, ied_lin
        if(fln_tce%id_fline_export(1,i) .eq. my_rank) then
          icou = icou + 1
        end if
      end do
!
      call calypso_mpi_allgather_one_int                                &
     &   (icou, fln_tce%num_current_fline)
!
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &                   = fln_tce%istack_current_fline(ip-1)           &
     &                    + fln_tce%num_current_fline(ip)
      end do
!
      icou =   fln_tce%istack_current_fline(my_rank)
      do i = 1, ied_lin
        if(fln_tce%id_fline_export(1,i) .eq. my_rank) then
          icou = icou + 1
          fln_tce%iflag_fline(icou) =  fln_tce%id_fline_export(2,i)
          fln_tce%icount_fline(icou) = fln_tce%id_fline_export(3,i)
          fln_tce%isf_fline_start(1:3,icou)                             &
     &         = fln_tce%id_fline_export(4:6,i)
!
          fln_tce%xx_fline_start(1:4,icou)                              &
     &         = fln_tce%fline_export(1:4,i)
          fln_tce%v_fline_start(1:4,icou) = fln_tce%fline_export(5:8,i)
          fln_tce%trace_length(icou) = fln_tce%fline_export(9,i)
          fln_tce%c_fline_start(1:fln_prm%ntot_color_comp,icou)         &
     &        = fln_tce%fline_export(9+1:9+fln_prm%ntot_color_comp,i)
        end if
      end do
!
      end subroutine set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      end module const_field_lines
