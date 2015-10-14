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
      use m_geometry_data
      use m_crs_matrix
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
       subroutine output_solution
!
       integer (kind = kint) :: i, ii, k
!
       write (*,*) 'SOLUTION FILE NAME: ', my_rank, solution_file_name
       open (id_file, file=solution_file_name, status='unknown')
!
       write (id_file,*) 'domain ID: ', my_rank
       write (id_file,*) 'node_id, solutions'
         do i= 1, node1%internal_node
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
      subroutine read_matrix_file
!
!
      write (*,*) 'INPUT FILE NAME: ', my_rank, matrix_file_name
      open (id_file, file=matrix_file_name, status='unknown')

      call read_size_of_crs_matrix

      call alloc_crs_matrix(node1%numnod, tbl1_crs, mat1_crs)

      call read_crs_matrix

      call read_communication_data
!
      close (id_file)
!
      end subroutine read_matrix_file
!
!  ---------------------------------------------------------------------
!
       subroutine read_size_of_crs_matrix
!
      use m_nod_comm_table
!
       read (id_file,'(10i16)') node1%internal_node, node1%numnod,      &
     &     tbl1_crs%ntot_l, tbl1_crs%ntot_u, mat1_crs%NB_crs,           &
     &     nod_comm%num_neib

!
       end subroutine read_size_of_crs_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine read_crs_matrix
!
       integer (kind = kint) :: i, k, j1, j2, kk, NB
!

      read (id_file,*) tbl1_crs%istack_l(1:node1%numnod)
      read (id_file,*) tbl1_crs%istack_u(1:node1%numnod)
      read (id_file,*) tbl1_crs%item_l(1:tbl1_crs%ntot_l)
      read (id_file,*) tbl1_crs%item_u(1:tbl1_crs%ntot_u)

      NB = mat1_crs%NB_crs
      if (NB .eq. 1) then
        read(id_file,*) mat1_crs%AL_crs(1,1,1:tbl1_crs%ntot_l)
        read(id_file,*) mat1_crs%AU_crs(1,1,1:tbl1_crs%ntot_u)
        
        read(id_file,*) (mat1_crs%D_crs(1,1,i), i= 1,node1%numnod)
        read(id_file,*) (mat1_crs%B_crs(i), i= 1,node1%numnod)
      else
        do  k= 1, tbl1_crs%ntot_l
          do j1= 1, NB
            read (id_file,*) (mat1_crs%AL_crs(j1,j2,k),j2= 1,NB)
          enddo
        enddo

        do  k= 1, tbl1_crs%ntot_u
          do j1= 1, NB
            read (id_file,*) (mat1_crs%AU_crs(j1,j2,k),j2= 1,NB)
          enddo
        enddo

        do  k= 1, node1%numnod
          do j1= 1, NB
            kk = NB*(k-1) + j1
            read (id_file,*)                                            &
     &        (mat1_crs%D_crs(j1,j2,k),j2= 1, NB), mat1_crs%B_crs(kk)
          enddo
        enddo
!
       end if
!
        do i= 1, (NB * node1%numnod)
          mat1_crs%X_crs(i)= 0.d0
        enddo
!
       do i = 1, node1%numnod
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
      subroutine read_communication_data
!
      use calypso_mpi
      use m_nod_comm_table
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
