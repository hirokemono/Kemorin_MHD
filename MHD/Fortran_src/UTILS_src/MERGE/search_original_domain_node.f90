!>@file   search_original_domain_node.f90
!!@brief  module search_original_domain_node
!!
!!@author H. Okuda and H. Matsui
!!@date  Programmed by H. MAtsui in June, 2018
!
!>@brief  find orignal node and domain address from global ID
!!
!!@verbatim
!!      subroutine search_node_by_global_id(irank_org,                  &
!!     &          org_node, new_node, inew_gl_sorted, inew_lc_sorted,   &
!!     &          irank_from_org, inod_from_org)
!!        type(node_data), intent(in) :: org_node
!!        type(node_data), intent(in) :: new_node
!!      subroutine set_comm_table_4_assemble                            &
!!     &         (nprocs_org, org_mesh, new_node,                       &
!!     &          irank_from_org, inod_from_org,                        &
!!     &          istack_recv, item_send, item_recv)
!!        type(node_data), intent(in) :: new_node
!!        type(mesh_geometry), intent(in) :: org_mesh(nprocs_org)
!!@endverbatim
!!
      module search_original_domain_node
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_mesh_data
      use t_geometry_data
!
      implicit none
!
      private :: find_node_by_global_id
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine search_node_by_global_id(irank_org,                    &
     &          org_node, new_node, inew_gl_sorted, inew_lc_sorted,     &
     &          irank_from_org, inod_from_org)
!
      use quicksort
!
      integer, intent(in) :: irank_org
      type(node_data), intent(in) :: org_node
      type(node_data), intent(in) :: new_node
!
      integer(kind = kint_gl), intent(in)                               &
     &                      :: inew_gl_sorted(new_node%internal_node)
      integer(kind = kint), intent(in)                                  &
     &                      :: inew_lc_sorted(new_node%internal_node)
!
      integer(kind = kint), intent(inout)                               &
     &                      :: irank_from_org(new_node%internal_node)
      integer(kind = kint), intent(inout)                               &
     &                      :: inod_from_org(new_node%internal_node)
!
      integer(kind = kint_gl), allocatable :: iorg_gl_sorted(:)
      integer(kind = kint), allocatable :: iorg_lc_sorted(:)
      integer(kind = kint), allocatable :: inod_to_new(:)
      integer(kind = kint) :: inod
!
      allocate(iorg_gl_sorted(org_node%internal_node))
      allocate(iorg_lc_sorted(org_node%internal_node))
      allocate(inod_to_new(org_node%internal_node))
!
!$omp parallel do
      do inod = 1, org_node%internal_node
        iorg_gl_sorted(inod) = org_node%inod_global(inod)
        iorg_lc_sorted(inod) = inod
        inod_to_new(inod) = 0
      end do
!$omp end parallel do
!
      call quicksort_int8_w_index                                       &
    &    (org_node%internal_node, iorg_gl_sorted,                       &
    &     ione, org_node%internal_node, iorg_lc_sorted)
!
      call find_node_by_global_id(irank_org,                            &
     &    org_node%internal_node, iorg_gl_sorted, iorg_lc_sorted,       &
     &    new_node%internal_node, inew_gl_sorted, inew_lc_sorted,       &
     &    irank_from_org, inod_from_org, inod_to_new)
!
      deallocate(inod_to_new)
!
      end subroutine search_node_by_global_id
!
! ----------------------------------------------------------------------
!
      subroutine find_node_by_global_id(irank_org,                      &
     &          internal_node_org, iorg_gl_sorted, iorg_lc_sorted,      &
     &          internal_node_new, inew_gl_sorted, inew_lc_sorted,      &
     &          irank_from_org, inod_from_org, inod_to_new)
!
      integer, intent(in) :: irank_org
      integer(kind = kint), intent(in) :: internal_node_org
      integer(kind = kint_gl), intent(in)                               &
     &                      :: iorg_gl_sorted(internal_node_org)
      integer(kind = kint), intent(in)                                  &
     &                      :: iorg_lc_sorted(internal_node_org)
!
      integer(kind = kint), intent(in) :: internal_node_new
      integer(kind = kint_gl), intent(in)                               &
     &                      :: inew_gl_sorted(internal_node_new)
      integer(kind = kint), intent(in)                                  &
     &                      :: inew_lc_sorted(internal_node_new)
!
      integer(kind = kint), intent(inout)                               &
     &                      :: irank_from_org(internal_node_new)
      integer(kind = kint), intent(inout)                               &
     &                      :: inod_from_org(internal_node_new)
      integer(kind = kint), intent(inout)                               &
     &                      :: inod_to_new(internal_node_new)
!
      integer(kind = kint) :: inod, ist, ied, imid, inew, iorg
!
!
!$omp parallel do private(inod,ist,ied,imid,inew,iorg)
      do inod = 1, internal_node_new
!        write(*,*) 'irank_from_org(inod)',                    &
!     &                     my_rank, inod, irank_from_org(inod)
        if(irank_from_org(inod) .ge. 0) cycle
!
        ist = 1
        ied = internal_node_org
        do
          imid = (ist + ied) / 2
!          if(my_rank .eq. 0 .and. inod .eq. 39)   &
!     &    write(50+my_rank,*) 'imid', inod, ist, ied, imid,            &
!     &        inew_gl_sorted(inod), iorg_gl_sorted(ist:ist+1),         &
!     &        iorg_gl_sorted(imid-1:imid+1), iorg_gl_sorted(ied-1:ied)
          if(inew_gl_sorted(inod) .lt. iorg_gl_sorted(ist)) then
            exit
          else if(inew_gl_sorted(inod) .gt. iorg_gl_sorted(ied)) then
            exit
          else if(inew_gl_sorted(inod) .eq. iorg_gl_sorted(ist)) then
            inew = inew_lc_sorted(inod)
            iorg = iorg_lc_sorted(ist)
            irank_from_org(inew) = irank_org
            inod_from_org(inew) =  iorg
            inod_to_new(iorg) =    inew
!            write(50+my_rank,*) 'hit', my_rank, inod
            exit
          else if(inew_gl_sorted(inod) .eq. iorg_gl_sorted(ied)) then
            inew =  inew_lc_sorted(inod)
            iorg = iorg_lc_sorted(ied)
            irank_from_org(inew) = irank_org
            inod_from_org(inew) =  iorg
            inod_to_new(iorg) =    inew
!            write(50+my_rank,*) 'hit', my_rank, inod
            exit
          else if(inew_gl_sorted(inod) .eq. iorg_gl_sorted(imid)) then
            inew =   inew_lc_sorted(inod)
            iorg = iorg_lc_sorted(imid)
            irank_from_org(inew) = irank_org
            inod_from_org(inew) =  iorg
            inod_to_new(iorg) =    inew
!            write(50+my_rank,*) 'hit', my_rank, inod
            exit
          else if( inew_gl_sorted(inod) .gt. iorg_gl_sorted(ist)        &
     &       .and. inew_gl_sorted(inod) .lt. iorg_gl_sorted(imid)) then
            ist = ist +  1
            ied = imid - 1
          else if( inew_gl_sorted(inod) .gt. iorg_gl_sorted(imid)       &
     &       .and. inew_gl_sorted(inod) .lt. iorg_gl_sorted(ied)) then
            ist = imid + 1
            ied = ied - 1
          else if(ist .ge. ied) then
            exit
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine find_node_by_global_id
!
! ----------------------------------------------------------------------
!
      subroutine set_comm_table_4_assemble                              &
     &         (nprocs_org, org_mesh, new_node,                         &
     &          irank_from_org, inod_from_org,                          &
     &          istack_recv, item_send, item_recv)
!
      integer, intent(in) :: nprocs_org
      type(node_data), intent(in) :: new_node
!
      type(mesh_geometry), intent(in) :: org_mesh(nprocs_org)
      integer(kind = kint), intent(in)                                  &
     &                      :: irank_from_org(new_node%internal_node)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_from_org(new_node%internal_node)
!
      integer(kind = kint), intent(inout) :: istack_recv(0:nprocs_org)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_send(new_node%internal_node)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_recv(new_node%internal_node)
!
      integer(kind = kint) :: inod, ip, icou, jp, jnod
      integer(kind = kint_gl) :: iflag8
      integer(kind = kint) :: ist, ied, inum, inod_org, inod_new
!
!
      icou = 0
      do inod = 1, new_node%internal_node
        if(irank_from_org(inod) .lt. 0) then
          if(icou .eq. 0) write(my_rank+50,*) 'Misiing node check'
          write(my_rank+50,*) 'failed: ',                               &
     &            my_rank, inod, new_node%inod_global(inod)
          icou = icou + 1
        end if
      end do
      write(*,*) 'Number of misiing node in rank ', my_rank, ': ', icou
!
      icou = 0
      do inod = 1, new_node%internal_node
        jp =   irank_from_org(inod) + 1
        jnod = inod_from_org(inod)
        iflag8 = new_node%inod_global(inod)                             &
     &        - org_mesh(jp)%node%inod_global(jnod)
        if(iflag8 .ne. 0) then
          if(icou .eq. 0) write(my_rank+50,*) 'Wrong node: '
          write(my_rank+50,*) 'Wrong node: ', my_rank, inod
          icou = icou + 1
        end if
      end do
      write(*,*) 'Number of wrong node in rank ', my_rank, ': ', icou
!
!
      istack_recv(0) = 0
      do ip = 1, nprocs_org
        icou = istack_recv(ip-1)
        do inod = 1, new_node%internal_node
          if(irank_from_org(inod) .eq. (ip-1)) then
            icou = icou + 1
            item_recv(icou) = inod
            item_send(icou) = inod_from_org(inod)
          end if
        end do
        istack_recv(ip) = icou
      end do
      call calypso_mpi_barrier
!
      icou = 0
      do ip = 1, nprocs_org
        ist = istack_recv(ip-1) + 1
        ied = istack_recv(ip)
        do inum = ist, ied
          inod_new = item_recv(inum)
          inod_org = item_send(inum)
          iflag8 = new_node%inod_global(inod_new)                       &
     &        - org_mesh(ip)%node%inod_global(inod_org)
          if(iflag8 .ne. 0) then
            if(icou .eq. 0) write(my_rank+50,*) 'Wrong comm. table: '
            write(my_rank+50,*) 'Wrong comm. table: ', my_rank, inod
            icou = icou + 1
          end if
        end do
      end do
      write(*,*) 'Number of wrong comm table in rank ', my_rank,        &
     &        ': ', icou
!
      end subroutine set_comm_table_4_assemble
!
! ----------------------------------------------------------------------
!
      end module search_original_domain_node
