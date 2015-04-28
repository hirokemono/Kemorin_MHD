!cal_delta_z_4_z_filter.f90
!      module cal_delta_z_4_z_filter
!
!      Written by H. Matsui
!
!      subroutine cal_delta_z
!
      module cal_delta_z_4_z_filter
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine cal_delta_z
!
      use m_geometry_parameter
      use m_crs_matrix
      use m_int_edge_vart_width
      use m_int_edge_data
      use m_commute_filter_z
!
      use calcs_by_LUsolver
      use solve_by_mass_z
      use int_edge_mass_mat_z_filter
      use set_matrices_4_z_filter
!
      integer(kind = kint) :: num_int
!
!
      num_int = i_int_z_filter
!
      call int_edge_mass_matrix(num_int)
!      iflag_mass = 0
!
      if ( iflag_mass .eq. 1) then
       call allocate_consist_mass_crs
       call set_consist_mass_mat
!
       call allocate_delta_z
       call int_edge_vart_width(num_int)
!
       call set_rhs_vart_width
       call set_consist_mass_mat
!
       write(*,*) METHOD_crs
       if ( METHOD_crs .eq. 'LU' ) then
!
         call solve_delta_z_LU
!
       else
!
         write(*,*) 'solve_crs_by_mass_z'
         call solve_crs_by_mass_z
         write(*,*) 'copy_solution_2_deltaz'
         call copy_solution_2_deltaz
!
        end if
!
       write(*,*) 'int_edge_diff_vart_w'
       call int_edge_diff_vart_w(num_int)
       write(*,*) 'set_rhs_vart_width'
       call set_rhs_vart_width

       if ( METHOD_crs .eq. 'LU' ) then
         call solve_delta_dz_LU
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2
         write(*,*) 'copy_solution_2_delta_dz'
         call copy_solution_2_delta_dz
        end if
!
       call int_edge_d2_vart_w(num_int)
!       call int_edge_d2_vart_w2(num_int)
       call set_rhs_vart_width

       if ( METHOD_crs .eq. 'LU' ) then
          call solve_delta_d2z_LU
       else
         write(*,*) 'solve_crs_by_mass_z2'
         call  solve_crs_by_mass_z2
         call copy_solution_2_d2dz
       end if
!
       call cal_sol_d2_vart_width
!
      else
        call allocate_delta_z
        call int_edge_vart_width(num_int)
        call cal_sol_vart_width
!
        call int_edge_diff_vart_w(num_int)
        call cal_sol_diff_vart_width
!
        call int_edge_d2_vart_w(num_int)
        call cal_sol_d2_vart_width
!
      end if
!
!
      end subroutine cal_delta_z
!
!   --------------------------------------------------------------------
!
      end module cal_delta_z_4_z_filter
