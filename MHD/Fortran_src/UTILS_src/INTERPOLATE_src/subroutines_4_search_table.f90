!
!     module subroutines_4_search_table
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine copy_position_2_2nd_local_ele(new_node, new_ele,     &
!!     &          iele, x_local)
!!      subroutine check_solution_in_tet(ref_error, s_coef)
!!
!!      subroutine check_missing_nodes                                  &
!!     &         (id_rank, nnod_dest, internod_dest, xx_dest,           &
!!     &          iflag_org_domain, ierr)
!
      module subroutines_4_search_table
!
      use m_precision
      use m_constants
!
      implicit none
!
      character(len=kchara), private, parameter                         &
     &                      :: miss_file_head = 'missing_node_list'
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine check_missing_nodes                                    &
     &         (id_rank, nnod_dest, internod_dest, xx_dest,             &
     &          iflag_org_domain, ierr)
!
      use set_parallel_file_name
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
!
      integer(kind = kint), intent(in) :: iflag_org_domain(nnod_dest)
      integer(kind = kint), intent(inout) ::ierr
!
      integer(kind= kint), parameter :: id_miss_file = 12
      character(len=kchara) :: miss_file_name
      integer(kind = kint) :: inod
!
!
      miss_file_name = add_process_id(id_rank, miss_file_head)
      open (id_miss_file, file = miss_file_name)
!
      ierr = 0
      write(id_miss_file,*) 'missing nodes: '
      do inod = 1, internod_dest
        if (iflag_org_domain(inod) .le. 0) then
          ierr = ierr + 1
          write(id_miss_file,'(i16,1p3e16.7)') inod, xx_dest(inod,1:3)
        end if
      end do
      close(id_miss_file)
!
      write(*,*) 'Number of missing nodes: ', ierr,                     &
     &          ' of  ', internod_dest, ' at rank ', id_rank
!
      end subroutine check_missing_nodes
!
!-----------------------------------------------------------------------
!
      end module subroutines_4_search_table
