!const_field_lines.f90
!
!      module const_field_lines
!
!      Written by H. Matsui on Aug., 2011
!
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
!!     &         (i_fln, node, ele, surf, ele_4_nod, nod_comm,          &
!!     &          fline_tce, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(communication_table), intent(in) :: nod_comm
!!        type(fieldline_trace), intent(inout) :: fline_tce
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
      use m_source_4_filed_line
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
      subroutine s_const_field_lines                                    &
     &         (i_fln, node, ele, surf, ele_4_nod, nod_comm,            &
     &          fline_tce, fline_lc)
!
!
      use m_control_params_4_fline
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_next_node_ele_4_node
      use t_local_fline
      use t_source_of_filed_line
      use extend_field_line
!
      integer(kind= kint), intent(in) :: i_fln
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(in) :: ele_4_nod
      type(communication_table), intent(in) :: nod_comm
!
      type(fieldline_trace), intent(inout) :: fline_tce
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: iflag_comm
      integer(kind = kint) :: i, ist, ied, num7, ip, src_rank, nline
!
!
      if(i_debug .gt. iflag_full_msg) then
        write(my_rank+50,*) 'num_all_fline',                            &
     &          fline_tce%num_all_fline(:,i_fln)
        write(my_rank+50,*)                                             &
     &         'istack_all_fline', fline_tce%istack_all_fline(:,i_fln)
        ist = fline_tce%istack_all_fline(my_rank,i_fln) + 1
        ied = fline_tce%istack_all_fline(my_rank+1,i_fln)
        write(my_rank+50,*) 'isf_fline_start(1:3,i)'
        do i = ist, ied
          write(my_rank+50,*) i, fline_tce%isf_fline_start(1:3,i)
        end do
      end if
      call calypso_MPI_barrier
!
      iflag_comm = 0
      call reset_fline_start(fline_lc)
!
      do
        ist = fline_tce%istack_all_fline(my_rank,i_fln) + 1
        ied = fline_tce%istack_all_fline(my_rank+1,i_fln)
        do i = ist, ied
          call s_extend_field_line                                      &
     &       (node%numnod, ele%numele, surf%numsurf,                    &
     &        surf%nnod_4_surf, node%xx, surf%ie_surf, surf%isf_4_ele,  &
     &        surf%iele_4_surf, surf%interior_surf, surf%vnorm_surf,    &
     &        max_line_stepping(i_fln), iflag_fline_used_ele(1,i_fln),  &
     &        fline_tce%iflag_fline(i), vector_nod_fline(1,1,i_fln),    &
     &        color_nod_fline(1,i_fln), fline_tce%isf_fline_start(1,i), &
     &        fline_tce%xx_fline_start(1,i),                            &
     &        fline_tce%v_fline_start(1,i),                             &
     &        fline_tce%c_fline_start(i), fline_tce%icount_fline(i),    &
     &        iflag_comm, fline_lc)
          write(50+my_rank,*) 'extension end for ', i, iflag_comm
!
          call set_fline_start_2_bcast(iflag_comm, i, node%numnod,      &
     &          ele%numele, node%inod_global, ele%iele_global,          &
     &          nod_comm%num_neib, nod_comm%id_neib,                    &
     &          nod_comm%ntot_import,  nod_comm%istack_import,          &
     &          nod_comm%item_import, fline_tce)
        end do
        call calypso_MPI_barrier
!
        do ip = 1, nprocs
          src_rank = ip - 1
          ist = fline_tce%istack_all_fline(ip-1,i_fln)
          num7 = 7*(fline_tce%istack_all_fline(ip,i_fln) - ist)
          if(num7 .gt. 0) then
            call mpi_Bcast(fline_tce%id_fline_export(1,ist+1), num7,    &
     &          CALYPSO_INTEGER, src_rank, CALYPSO_COMM, ierr_MPI)
            call mpi_Bcast(fline_tce%fline_export(1,ist+1), num7,       &
     &          CALYPSO_REAL, src_rank, CALYPSO_COMM, ierr_MPI)
          end if
        end do
!
        if(iflag_debug .gt. 0) then
          ist = fline_tce%istack_all_fline(0,i_fln) + 1
          ied = fline_tce%istack_all_fline(nprocs,i_fln)
          write(my_rank+50,*)                                           &
     &        'i, new_start_pe, iflag_fline, new_start_cont'
          do i = ist, ied
            write(my_rank+50,'(10i16)')                                 &
     &              i, fline_tce%id_fline_export(1:3,i)
          end do
        end if
!
        call recover_local_fline_start(i_fln,                           &
     &      node%numnod, ele%numele, surf%numsurf, ele%iele_global,     &
     &      surf%isf_4_ele, surf%iele_4_surf, ele_4_nod%ntot,           &
     &      ele_4_nod%istack_4_node, ele_4_nod%iele_4_node,             &
     &      nod_comm%num_neib, nod_comm%id_neib, nod_comm%ntot_export,  &
     &      nod_comm%istack_export, nod_comm%item_export, fline_tce)
        call set_fline_start_from_neib(i_fln, fline_tce)
!
        nline = fline_tce%istack_all_fline(nprocs,i_fln)                &
     &         - fline_tce%istack_all_fline(0,i_fln)
        if(i_debug .gt. 0) then
          write(my_rank+50,*) 'istack_all_fline',                       &
     &                       fline_tce%istack_all_fline(:,i_fln)
!
          write(my_rank+50,*) 'number of lines: ', nline
          write(*,*) 'number of lines: ', my_rank, nline
        end if
        if(nline .le. 0) exit
      end do
!
!      call check_local_fline_dx( (my_rank+60+i_fln*100), fline_lc)
!
      end subroutine s_const_field_lines
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_2_bcast(iflag_comm, iline,             &
     &          numnod, numele, inod_global, iele_global,               &
     &          num_neib, id_neib, ntot_import, istack_import,          &
     &          item_import, fline_tce)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: iflag_comm, iline
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint_gl), intent(in) :: iele_global(numele)
!
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: item_import(ntot_import)
!
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: inod, iele, isf, ip, ist, ied, inum
!
!
      if(iflag_comm .eq. ione) then
        iele = fline_tce%isf_fline_start(1,iline)
        isf =  fline_tce%isf_fline_start(2,iline)
        inod = fline_tce%isf_fline_start(3,iline)
        do ip = 1, num_neib
          ist = istack_import(ip-1) + 1
          ied = istack_import(ip)
          do inum = ist, ied
            if(item_import(inum) .eq. inod) then
              fline_tce%id_fline_export(1,iline) = id_neib(ip)
              fline_tce%id_fline_export(7,iline) = 1 + inum - ist
              exit
            end if
          end do
        end do
!
        fline_tce%id_fline_export(2,iline)                              &
     &      = fline_tce%iflag_fline(iline)
        fline_tce%id_fline_export(3,iline)                              &
     &      = fline_tce%icount_fline(iline)
        fline_tce%id_fline_export(4,iline) = int(iele_global(iele))
        fline_tce%id_fline_export(5,iline) = isf
        fline_tce%id_fline_export(6,iline) = int(inod_global(inod))
!
        fline_tce%fline_export(1:3,iline)                               &
     &      = fline_tce%xx_fline_start(1:3,iline)
        fline_tce%fline_export(4:6,iline)                               &
     &      = fline_tce%v_fline_start(1:3,iline)
        fline_tce%fline_export(7,iline)                                 &
     &      = fline_tce%c_fline_start(iline)
      else
        fline_tce%id_fline_export(1,iline) =   -ione
        fline_tce%id_fline_export(2:7,iline) = izero
        fline_tce%fline_export(1:7,iline) =     zero
      end if
!
      end subroutine set_fline_start_2_bcast
!
!  ---------------------------------------------------------------------
!
      subroutine recover_local_fline_start(i_fln,                       &
     &          numnod, numele, numsurf, iele_global,                   &
     &          isf_4_ele, iele_4_surf, ntot_ele_4_node,                &
     &          iele_stack_4_node, iele_4_node, num_neib, id_neib,      &
     &          ntot_export, istack_export, item_export, fline_tce)
!
      use m_geometry_constants
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i_fln
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer (kind=kint_gl), intent(in) :: iele_global(numele)
      integer (kind=kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer (kind=kint), intent(in) :: iele_4_surf(numsurf,2,2)
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
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: ip, ip_org, ist_lin, ied_lin, iline
      integer(kind = kint) :: inum, inod, ist_ele, ied_ele, jnum, jele
      integer(kind = kint) :: isf, isurf
!
!
      do ip = 1, num_neib
        ip_org = id_neib(ip) + 1
!
        ist_lin = fline_tce%istack_all_fline(ip_org-1,i_fln) + 1
        ied_lin = fline_tce%istack_all_fline(ip_org,i_fln)
        do iline = ist_lin, ied_lin
          if(fline_tce%id_fline_export(1,iline) .eq. my_rank) then
            inum = fline_tce%id_fline_export(7,iline)                   &
     &            + istack_export(ip-1)
            inod = item_export(inum)
!            write(60+my_rank,*) 'recover node', inod,                  &
!     &           inod_global(inod),fline_tce%id_fline_export(6,iline)
!
            fline_tce%id_fline_export(6,iline) = inod
            ist_ele = iele_stack_4_node(inod-1) + 1
            ied_ele = iele_stack_4_node(inod)
            do jnum = ist_ele, ied_ele
              jele = iele_4_node(jnum)
              if(iele_global(jele)                                      &
     &               .eq. fline_tce%id_fline_export(4,iline)) then
!                write(60+my_rank,*) 'recover ele',                     &
!      &                      jele, iele_global(jele)
                isf =  fline_tce%id_fline_export(5,iline)
                isurf = abs(isf_4_ele(jele,isf))
!
                if(isf_4_ele(jele,isf) .lt. 0) then
                  fline_tce%id_fline_export(4,iline)                    &
     &                  = iele_4_surf(isurf,1,1)
                  fline_tce%id_fline_export(5,iline)                    &
     &                  = iele_4_surf(isurf,1,2)
                else
                  fline_tce%id_fline_export(4,iline)                    &
     &                  = iele_4_surf(isurf,2,1)
                  fline_tce%id_fline_export(5,iline)                    &
     &                  = iele_4_surf(isurf,2,2)
                end if
!                write(60+my_rank,*) 'recover surf',                    &
!     &                         fline_tce%id_fline_export(4:5,iline)
!
                exit
              end if
            end do
!
          end if
        end do
      end do
!
      end subroutine recover_local_fline_start
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_from_neib(i_fln, fline_tce)
!
      use t_source_of_filed_line
!
      integer(kind = kint), intent(in) :: i_fln
      type(fieldline_trace), intent(inout) :: fline_tce
!
      integer(kind = kint) :: ist_lin, ied_lin, iline, icou, ip
!
!
      ist_lin = fline_tce%istack_all_fline(0,i_fln) + 1
      ied_lin = fline_tce%istack_all_fline(nprocs,i_fln)
      icou = 0
      do iline = ist_lin, ied_lin
        if(fline_tce%id_fline_export(1,iline) .eq. my_rank) then
          icou = icou + 1
        end if
      end do
!
      call MPI_AllGather(icou, ione, CALYPSO_INTEGER,                   &
     &    fline_tce%num_all_fline(1,i_fln), ione, CALYPSO_INTEGER,      &
     &    CALYPSO_COMM, ierr_MPI)
!
      do ip = 1, nprocs
        fline_tce%istack_all_fline(ip,i_fln)                            &
     &                   = fline_tce%istack_all_fline(ip-1,i_fln)       &
     &                    + fline_tce%num_all_fline(ip,i_fln)
      end do
!
      icou = fline_tce%istack_all_fline(my_rank,i_fln)
      do iline = ist_lin, ied_lin
        if(fline_tce%id_fline_export(1,iline) .eq. my_rank) then
          icou = icou + 1
          fline_tce%iflag_fline(icou)                                   &
     &         = fline_tce%id_fline_export(2,iline)
          fline_tce%icount_fline(icou)                                  &
     &         = fline_tce%id_fline_export(3,iline)
          fline_tce%isf_fline_start(1:3,icou)                           &
     &         = fline_tce%id_fline_export(4:6,iline)
!
          fline_tce%xx_fline_start(1:3,icou)                            &
     &         = fline_tce%fline_export(1:3,iline)
          fline_tce%v_fline_start(1:3,icou)                             &
     &         = fline_tce%fline_export(4:6,iline)
          fline_tce%c_fline_start(icou)                                 &
     &         = fline_tce%fline_export(7,iline)
        end if
      end do
!
      end subroutine set_fline_start_from_neib
!
!  ---------------------------------------------------------------------
!
      end module const_field_lines
