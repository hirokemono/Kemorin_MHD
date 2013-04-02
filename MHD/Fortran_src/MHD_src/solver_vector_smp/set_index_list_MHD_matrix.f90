!
!     module set_index_list_MHD_matrix
!
!      Written by H. Matsui
!
!
!      subroutine set_index_list_4_matrix
!
!
      module set_index_list_MHD_matrix
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_index_list_4_matrix
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_sorted_node
      use m_sorted_node_MHD
!
      use set_idx_list_quad_mat
      use set_idx_list_linear_mat
!
!
!      write(*,*) 'allocate_marix_list'
      call allocate_marix_list
!
!      write(*,*) 'set_index_list_4_mat_etr'
      call set_index_list_4_mat_etr
!      write(*,*) 'set_index_list_4_mat_etr_l'
      call set_index_list_4_mat_etr_l
!
      if (iflag_t_evo_4_velo.ne.0 .or. iflag_t_evo_4_temp.ne.0          &
     &  .or. iflag_t_evo_4_composit .ne. 0) then
!        write(*,*) 'allocate_marix_list_fl'
        call allocate_marix_list_fl
!
!        write(*,*) 'set_index_list_4_mat_fl'
        call set_index_list_4_mat_fl
!        write(*,*) 'set_index_list_4_mat_fl_l'
        call set_index_list_4_mat_fl_l
      end if
!
      if (iflag_t_evo_4_magne.ne.0 .or. iflag_t_evo_4_vect_p.eq.4) then
!        write(*,*) 'allocate_marix_list_cd'
        call allocate_marix_list_cd
!        write(*,*) 'allocate_marix_list_ins'
!        call allocate_marix_list_ins
!
        write(*,*) 'set_index_list_4_mat_cd'
        call set_index_list_4_mat_cd
!        write(*,*) 'set_index_list_4_mat_ins'
!        call set_index_list_4_mat_ins
!
!        write(*,*) 'set_index_list_4_mat_cd_l'
!        call set_index_list_4_mat_cd_l
!        write(*,*) 'set_index_list_4_mat_ins_l'
!        call set_index_list_4_mat_ins_l
      end if
!
!
      end subroutine set_index_list_4_matrix
!
! ----------------------------------------------------------------------
!
      end module set_index_list_MHD_matrix
