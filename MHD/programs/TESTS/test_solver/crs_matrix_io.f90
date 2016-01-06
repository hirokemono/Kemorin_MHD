!
!     module crs_matrix_io
!
!     Written by H. Matsui
!
!      subroutine output_solution
!      subroutine read_matrix_file
!
      module crs_matrix_io
!
      use m_precision
!
      use calypso_mpi
      use m_crs_matrix
!
      use t_geometry_data
      use t_comm_table
!
      implicit none
!
      integer(kind = kint), parameter :: id_file = 15
!
      character(len = kchara) :: matrix_file_head =   "matIN"
      character(len = kchara) :: solution_file_head = "solution"
!
      character(len = kchara) :: matrix_file_name
      character(len = kchara) :: solution_file_name
!
      private :: id_file
      private :: read_size_of_crs_matrix
      private :: read_crs_matrix, read_communication_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine output_solution(node)
!
      type(node_data), intent(in) :: node
       integer (kind = kint) :: i, ii, k
!
       write (*,*) 'SOLUTION FILE NAME: ', my_rank, solution_file_name
       open (id_file, file=solution_file_name, status='unknown')
!
       write (id_file,*) 'domain ID: ', my_rank
       write (id_file,*) 'node_id, solutions'
         do i= 1, node%internal_node
           ii = mat1_crs%NB_crs*(i-1)
           write (id_file,'(i16,100(1pe23.12))') i,                     &
     &            (mat1_crs%X_crs(ii+k),k=1,mat1_crs%NB_crs)
         end do
!
       close (id_file)
!
       end  subroutine output_solution
!
!  ---------------------------------------------------------------------
!
      subroutine read_matrix_file(nod_comm, node)
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
!
!
      write (*,*) 'INPUT FILE NAME: ', my_rank, matrix_file_name
      open (id_file, file=matrix_file_name, status='unknown')

      call read_size_of_crs_matrix(nod_comm, node)

      call alloc_crs_matrix(node%numnod, tbl1_crs, mat1_crs)

      call read_crs_matrix(node)

      call read_communication_data(nod_comm)
!
      close (id_file)
!
      end subroutine read_matrix_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_size_of_crs_matrix(nod_comm, node)
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
!
!
       read (id_file,'(10i16)') node%internal_node, node%numnod,        &
     &     tbl1_crs%ntot_l, tbl1_crs%ntot_u, mat1_crs%NB_crs,           &
     &     nod_comm%num_neib
!
       end subroutine read_size_of_crs_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine read_crs_matrix(node)
!
      type(node_data), intent(in) :: node
!
       integer (kind = kint) :: i, k, j1, kk, jst, jed, NB
!

      read (id_file,*) tbl1_crs%istack_l(1:node%numnod)
      read (id_file,*) tbl1_crs%istack_u(1:node%numnod)
      read (id_file,*) tbl1_crs%item_l(1:tbl1_crs%ntot_l)
      read (id_file,*) tbl1_crs%item_u(1:tbl1_crs%ntot_u)

      NB = mat1_crs%NB_crs
      if (NB .eq. 1) then
        read(id_file,*) mat1_crs%AL_crs(1:tbl1_crs%ntot_l)
        read(id_file,*) mat1_crs%AU_crs(1:tbl1_crs%ntot_u)
        
        read(id_file,*) mat1_crs%D_crs(1:node%numnod)
        read(id_file,*) mat1_crs%B_crs(1:node%numnod)
      else
        do  k= 1, tbl1_crs%ntot_l
          do j1= 1, NB
            jst = j1 + (k-1) * NB*NB
            jed = j1 + (NB-1) * NB + (k-1) * NB*NB
            read (id_file,*) mat1_crs%AL_crs(jst:jed:NB)
          enddo
        enddo

        do  k= 1, tbl1_crs%ntot_u
          do j1= 1, NB
            jst = j1 + (k-1) * NB*NB
            jed = j1 + (NB-1) * NB + (k-1) * NB*NB
            read (id_file,*) mat1_crs%AU_crs(jst:jed:NB)
          enddo
        enddo

        do  k= 1, node%numnod
          do j1= 1, NB
            jst = j1 + (k-1) * NB*NB
            jed = j1 + (NB-1) * NB + (k-1) * NB*NB
            kk = NB*(k-1) + j1
            read (id_file,*)                                            &
     &             mat1_crs%D_crs(jst:jed:NB), mat1_crs%B_crs(kk)
          enddo
        enddo
!
       end if
!
        do i= 1, (NB * node%numnod)
          mat1_crs%X_crs(i)= 0.d0
        enddo
!
       do i = 1, node%numnod
         tbl1_crs%nitem_l(i) = tbl1_crs%istack_l(i)                     &
     &                        - tbl1_crs%istack_l(i-1)
         tbl1_crs%nitem_u(i) = tbl1_crs%istack_u(i)                     &
     &                        - tbl1_crs%istack_u(i-1)
       end do
!
       end subroutine read_crs_matrix
!
!  ---------------------------------------------------------------------
!
      subroutine read_communication_data(nod_comm)
!
      use calypso_mpi
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      call allocate_type_comm_tbl_num(nod_comm)
!
      if(nod_comm%num_neib .gt. 0) then
        read (id_file,*) nod_comm%id_neib(1:nod_comm%num_neib)
        read (id_file,*) nod_comm%istack_import(1:nod_comm%num_neib)
        read (id_file,*) nod_comm%istack_export(1:nod_comm%num_neib)
      end if

      nod_comm%ntot_import= nod_comm%istack_import(nod_comm%num_neib)
      nod_comm%ntot_export= nod_comm%istack_export(nod_comm%num_neib)

      call allocate_type_comm_tbl_item(nod_comm)
!
      if(nod_comm%ntot_import .gt. 0) then
        read (id_file,*) nod_comm%item_import(1:nod_comm%ntot_import)
      end if
      if(nod_comm%ntot_export .gt. 0) then
        read (id_file,*) nod_comm%item_export(1:nod_comm%ntot_export)
      end if
!
      end subroutine read_communication_data
!
!  ---------------------------------------------------------------------
!
      end module crs_matrix_io
